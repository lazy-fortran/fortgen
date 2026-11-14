module fortgen_tile_kernel
    ! Register-tile kernels for packed dense products: acc = A B^T for packed
    ! panels A(mr, k) and B(nr, k), both stored contiguously per step of k,
    ! and acc(mr, nr) column-major. This is the inner kernel of a GEMM in the
    ! GotoBLAS/BLIS design; the caller packs, blocks for the caches and adds
    ! acc to C.
    !
    ! A tile specification is emitted as assembly for one instruction set,
    ! wrapped in a C translation unit as a top-level asm block that is
    ! compiled only on its architecture; every C compiler our builds use (gcc,
    ! clang, emscripten, icx, nvc) accepts it. There is no Fortran fallback:
    ! measured against the assembly it lost up to 45 % (NEON, wide tiles),
    ! and an architecture without an emitter is a build error, not a slow
    ! path. The kernels hold the tile in vector registers, broadcast B, load
    ! A with one instruction per vector and unroll k.
    !
    ! C prototypes of the emitted kernels: the tile alone, or added to C in
    ! place (update_c), C column-major with leading dimension ldc; the caller
    ! folds alpha into a packed operand and applies beta before the product:
    !     void name(int64_t k, const double *a, const double *b, double *acc);
    !     void name(int64_t k, const double *a, const double *b, double *c, int64_t ldc);
    ! With a_strided, A is read unpacked from a column-major matrix:
    !     void name(int64_t k, const double *a, int64_t lda, const double *b,
    !               double *c_or_acc, int64_t ldc);
    ! With b_strided, B is read unpacked from a column-major matrix:
    !     void name(int64_t k, const double *a, const double *b, int64_t ldb,
    !               double *c_or_acc, int64_t ldc);
    ! aarch64_amx targets the matrix coprocessor of Apple M1 to M3: the tile
    ! lives in its Z registers instead of vector registers (see amx_body).
    implicit none
    private

    public :: tile_kernel_spec_t
    public :: emit_tile_kernel_c_asm
    public :: tile_kernel_check, tile_register_count
    public :: ISA_X86_64_AVX2_FMA, ISA_X86_64_AVX512
    public :: ISA_AARCH64_NEON, ISA_AARCH64_AMX, isa_name, isa_vector_doubles, isa_vector_registers

    integer, parameter :: ISA_X86_64_AVX2_FMA = 1
    integer, parameter :: ISA_X86_64_AVX512 = 2
    integer, parameter :: ISA_AARCH64_NEON = 3
    integer, parameter :: ISA_AARCH64_AMX = 4

    character(len=1), parameter :: nl = new_line('a')

    type :: tile_kernel_spec_t
        character(:), allocatable :: name
        integer :: isa = ISA_X86_64_AVX2_FMA
        !> Rows of the tile; a multiple of the vector length in doubles.
        integer :: mr = 8
        !> Columns of the tile.
        integer :: nr = 6
        !> Steps of k per loop iteration in the main loop.
        integer :: k_unroll = 4
        !> Prefetch distance into the A panel in bytes (0: none).
        integer :: prefetch_a = 0
        !> Add the tile to C(ldc, *) in place instead of storing it to acc.
        logical :: update_c = .false.
        !> Read A unpacked from a column-major matrix with leading dimension
        !> lda (one column of the tile per step of k) instead of a packed panel.
        logical :: a_strided = .false.
        !> Read B unpacked from a column-major matrix with leading dimension
        !> ldb (one row of the tile's columns per step of k); A stays packed.
        logical :: b_strided = .false.
    end type tile_kernel_spec_t

contains

    pure function isa_name(isa) result(name)
        integer, intent(in) :: isa
        character(:), allocatable :: name
        select case (isa)
        case (ISA_X86_64_AVX2_FMA)
            name = 'x86_64_avx2_fma'
        case (ISA_X86_64_AVX512)
            name = 'x86_64_avx512'
        case (ISA_AARCH64_NEON)
            name = 'aarch64_neon'
        case (ISA_AARCH64_AMX)
            name = 'aarch64_amx'
        case default
            name = 'unknown'
        end select
    end function isa_name

    pure integer function isa_vector_doubles(isa) result(width)
        integer, intent(in) :: isa
        select case (isa)
        case (ISA_X86_64_AVX2_FMA)
            width = 4
        case (ISA_X86_64_AVX512)
            width = 8
        case (ISA_AARCH64_NEON)
            width = 2
        case (ISA_AARCH64_AMX)
            width = 8
        case default
            width = 1
        end select
    end function isa_vector_doubles

    pure integer function isa_vector_registers(isa) result(count)
        integer, intent(in) :: isa
        select case (isa)
        case (ISA_X86_64_AVX2_FMA)
            count = 16
        case (ISA_X86_64_AVX512, ISA_AARCH64_NEON)
            count = 32
        case (ISA_AARCH64_AMX)
            ! fp64 accumulators of 8 x 8 in Z
            count = 8
        case default
            count = huge(1)
        end select
    end function isa_vector_registers

    !> Vector registers the tile occupies: accumulators, A vectors and the
    !> registers holding B (one broadcast register on x86, B packed two per
    !> register on NEON). AVX-512 tiles one vector tall broadcast from memory
    !> inside the FMA; taller ones broadcast into a register once per element
    !> of B, because a memory broadcast per FMA makes the loop load-bound
    !> (24x8 on Zen 5: 105 instead of 122 GFLOP/s).
    pure integer function tile_register_count(spec) result(count)
        type(tile_kernel_spec_t), intent(in) :: spec
        integer :: mv
        mv = spec%mr/isa_vector_doubles(spec%isa)
        select case (spec%isa)
        case (ISA_X86_64_AVX2_FMA)
            count = mv*spec%nr + mv + 1
        case (ISA_X86_64_AVX512)
            count = mv*spec%nr + mv + merge(0, 1, mv == 1)
        case (ISA_AARCH64_NEON)
            count = mv*spec%nr + mv + (spec%nr + 1)/2
        case (ISA_AARCH64_AMX)
            count = mv*(spec%nr/8)
        case default
            count = 0
        end select
    end function tile_register_count

    !> Empty message when the specification can be emitted, else the reason.
    function tile_kernel_check(spec) result(message)
        type(tile_kernel_spec_t), intent(in) :: spec
        character(:), allocatable :: message
        integer :: width
        message = ''
        if (.not. allocated(spec%name)) then
            message = 'tile kernel has no name'
        else if (len(spec%name) == 0) then
            message = 'tile kernel has no name'
        else if (spec%mr < 1 .or. spec%nr < 1) then
            message = 'tile shape must be positive'
        else if (spec%k_unroll < 1 .or. spec%k_unroll > 16) then
            message = 'k unroll must lie in 1..16'
        else if (spec%prefetch_a < 0) then
            message = 'prefetch distance must not be negative'
        else if (spec%isa < ISA_X86_64_AVX2_FMA .or. spec%isa > ISA_AARCH64_AMX) then
            message = 'unknown instruction set'
        else
            width = isa_vector_doubles(spec%isa)
            if (mod(spec%mr, width) /= 0) then
                message = 'mr must be a multiple of the vector length of '//isa_name(spec%isa)
            else if (spec%isa == ISA_AARCH64_NEON .and. mod(spec%nr, 2) /= 0) then
                message = 'nr must be even on aarch64_neon (B is loaded in pairs)'
            else if (spec%isa == ISA_AARCH64_AMX .and. mod(spec%nr, 8) /= 0) then
                message = 'nr must be a multiple of 8 on aarch64_amx'
            else if (spec%isa == ISA_AARCH64_AMX .and. spec%b_strided) then
                message = 'aarch64_amx loads B rows of 8 contiguous doubles; it cannot read B unpacked'
            else if (tile_register_count(spec) > isa_vector_registers(spec%isa)) then
                message = 'tile needs more vector registers than '//isa_name(spec%isa)//' has'
            else if (spec%a_strided .and. spec%b_strided) then
                message = 'A and B cannot both be read unpacked'
            else if (spec%b_strided .and. spec%nr > merge(6, 9, spec%isa == ISA_AARCH64_NEON)) then
                message = 'unpacked B supports at most 9 columns (6 on aarch64_neon)'
            end if
        end if
    end function tile_kernel_check

    !> C translation unit with the kernel as a top-level asm block, compiled
    !> only on the kernel's architecture (System V on x86_64 except Windows,
    !> AAPCS64 on aarch64; Mach-O symbols get the leading underscore).
    function emit_tile_kernel_c_asm(spec, ok, message) result(source)
        type(tile_kernel_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character(:), allocatable :: source, body, guard

        message = tile_kernel_check(spec)
        ok = len(message) == 0
        source = ''
        if (.not. ok) return
        select case (spec%isa)
        case (ISA_X86_64_AVX2_FMA, ISA_X86_64_AVX512)
            guard = 'defined(__x86_64__) && !defined(_WIN32)'
            body = x86_body(spec)
        case (ISA_AARCH64_NEON)
            guard = 'defined(__aarch64__) && !defined(_WIN32)'
            body = neon_body(spec)
        case (ISA_AARCH64_AMX)
            guard = 'defined(__aarch64__) && defined(__APPLE__)'
            body = amx_body(spec)
        end select
        call add(source, '/* '//spec%name//': '//isa_name(spec%isa)//' tile '//itoa(spec%mr)//'x'// &
            itoa(spec%nr)//', k unrolled '//itoa(spec%k_unroll)//'. */')
        call add(source, '#include <stdint.h>')
        call add(source, '#if '//guard)
        call add(source, '#if defined(__APPLE__)')
        call add(source, '#define FG_SYM(name) "_" #name')
        call add(source, '#define FG_L "L"')
        call add(source, '#define FG_TYPE(name) ""')
        call add(source, '#define FG_SIZE(name) ""')
        call add(source, '#else')
        call add(source, '#define FG_SYM(name) #name')
        call add(source, '#define FG_L ".L"')
        ! ELF type and size, so that profilers attribute samples to the kernel
        ! and not to whatever symbol precedes the code after it
        call add(source, '#define FG_TYPE(name) ".type " #name ", '// &
            merge('%function', '@function', spec%isa >= ISA_AARCH64_NEON)//'\n"')
        call add(source, '#define FG_SIZE(name) ".size " #name ", .-" #name "\n"')
        call add(source, '#endif')
        if (spec%a_strided) then
            call add(source, 'void '//spec%name//'(int64_t k, const double *a, int64_t lda, const double *b, '// &
                'double *'//merge('c  ', 'acc', spec%update_c)//', int64_t ldc);')
        else if (spec%b_strided) then
            call add(source, 'void '//spec%name//'(int64_t k, const double *a, const double *b, int64_t ldb, '// &
                'double *'//merge('c  ', 'acc', spec%update_c)//', int64_t ldc);')
        else if (spec%update_c) then
            call add(source, 'void '//spec%name//'(int64_t k, const double *a, const double *b, double *c, int64_t ldc);')
        else
            call add(source, 'void '//spec%name//'(int64_t k, const double *a, const double *b, double *acc);')
        end if
        call add(source, '__asm__(')
        call add(source, '    ".text\n"')
        call add(source, '    ".p2align 5\n"')
        call add(source, '    ".globl " FG_SYM('//spec%name//') "\n"')
        call add(source, '    FG_TYPE('//spec%name//')')
        call add(source, '    FG_SYM('//spec%name//') ":\n"')
        source = source//body
        call add(source, '    FG_SIZE('//spec%name//')')
        call add(source, ');')
        call add(source, '#endif')
    end function emit_tile_kernel_c_asm

    ! x86_64: rdi = k, rsi = a, rdx = b, rcx = acc. Accumulator (v, j) is
    ! register v + mv*j; A vectors follow; AVX2 broadcasts B into the last
    ! register, AVX-512 broadcasts from memory inside the FMA.
    function x86_body(spec) result(text)
        type(tile_kernel_spec_t), intent(in) :: spec
        character(:), allocatable :: text, r, lab
        integer :: width, mv, nacc, v, j, bytes
        logical :: wide

        wide = spec%isa == ISA_X86_64_AVX512
        width = isa_vector_doubles(spec%isa)
        bytes = 8*width
        mv = spec%mr/width
        nacc = mv*spec%nr
        r = merge('%zmm', '%ymm', wide)
        lab = spec%name
        text = ''
        do v = 0, nacc - 1
            call asm(text, 'vxorpd '//reg(v)//', '//reg(v)//', '//reg(v))
        end do
        if (spec%b_strided) then
            ! (k, a, b, ldb, c, ldc) in (rdi, rsi, rdx, rcx, r8, r9): ldb bytes in
            ! r12; column groups 0-2, 3-5, 6-8 of B from rdx, r13, r14
            call asm(text, 'push %r12')
            call asm(text, 'push %r13')
            call asm(text, 'push %r14')
            call asm(text, 'mov %rcx, %r12')
            call asm(text, 'shl $3, %r12')
            call asm(text, 'lea (%rdx,%r12,2), %r13')
            call asm(text, 'add %r12, %r13')
            call asm(text, 'lea (%r13,%r12,2), %r14')
            call asm(text, 'add %r12, %r14')
            call asm(text, 'mov %r8, %rcx')
            call asm(text, 'mov %r9, %r11')
        end if
        if (spec%a_strided) then
            ! (k, a, lda, b, c, ldc) in (rdi, rsi, rdx, rcx, r8, r9) to the
            ! registers below: lda in bytes in rdi once k is in rax
            call asm(text, 'mov %rdi, %rax')
            call asm(text, 'mov %rdx, %rdi')
            call asm(text, 'shl $3, %rdi')
            call asm(text, 'mov %rcx, %rdx')
            call asm(text, 'mov %r8, %rcx')
            call asm(text, 'mov %r9, %r11')
        end if
        ! rax = k / k_unroll, r9 = k mod k_unroll; div clobbers rdx (b)
        ! update_c: ldc arrives in r8, which the division below overwrites
        if (spec%update_c .and. .not. (spec%a_strided .or. spec%b_strided)) call asm(text, 'mov %r8, %r11')
        call asm(text, 'mov %rdx, %r10')
        if (.not. spec%a_strided) call asm(text, 'mov %rdi, %rax')
        call asm(text, 'xor %edx, %edx')
        call asm(text, 'mov $'//itoa(spec%k_unroll)//', %r8')
        call asm(text, 'div %r8')
        call asm(text, 'mov %rdx, %r9')
        call asm(text, 'mov %r10, %rdx')
        call asm(text, 'test %rax, %rax')
        call asm(text, 'jz .L'//lab//'_tail')
        call label(text, '.L'//lab//'_main')
        call steps(text, spec%k_unroll)
        if (.not. spec%a_strided) call asm(text, 'add $'//itoa(8*spec%mr*spec%k_unroll)//', %rsi')
        call advance_b(text, spec%k_unroll)
        call asm(text, 'dec %rax')
        call asm(text, 'jnz .L'//lab//'_main')
        call label(text, '.L'//lab//'_tail')
        call asm(text, 'test %r9, %r9')
        call asm(text, 'jz .L'//lab//'_store')
        call label(text, '.L'//lab//'_one')
        call steps(text, 1)
        if (.not. spec%a_strided) call asm(text, 'add $'//itoa(8*spec%mr)//', %rsi')
        call advance_b(text, 1)
        call asm(text, 'dec %r9')
        call asm(text, 'jnz .L'//lab//'_one')
        call label(text, '.L'//lab//'_store')
        if (spec%update_c) then
            ! C(:, j) += tile(:, j), column pointer in rax, ldc in bytes in r11
            call asm(text, 'shl $3, %r11')
            call asm(text, 'mov %rcx, %rax')
            do j = 0, spec%nr - 1
                do v = 0, mv - 1
                    call asm(text, 'vaddpd '//itoa(bytes*v)//'(%rax), '//reg(v + mv*j)//', '//reg(v + mv*j))
                    call asm(text, 'vmovupd '//reg(v + mv*j)//', '//itoa(bytes*v)//'(%rax)')
                end do
                if (j < spec%nr - 1) call asm(text, 'add %r11, %rax')
            end do
        else
            do j = 0, spec%nr - 1
                do v = 0, mv - 1
                    call asm(text, 'vmovupd '//reg(v + mv*j)//', '//itoa(8*(width*v + spec%mr*j))//'(%rcx)')
                end do
            end do
        end if
        call asm(text, 'vzeroupper')
        if (spec%b_strided) then
            call asm(text, 'pop %r14')
            call asm(text, 'pop %r13')
            call asm(text, 'pop %r12')
        end if
        call asm(text, 'ret')

    contains

        ! B moves on by count steps: one row of a packed panel, or 8 bytes in
        ! each column group of an unpacked B
        subroutine advance_b(t, count)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: count
            if (spec%b_strided) then
                call asm(t, 'add $'//itoa(8*count)//', %rdx')
                if (spec%nr > 3) call asm(t, 'add $'//itoa(8*count)//', %r13')
                if (spec%nr > 6) call asm(t, 'add $'//itoa(8*count)//', %r14')
            else
                call asm(t, 'add $'//itoa(8*spec%nr*count)//', %rdx')
            end if
        end subroutine advance_b

        ! address of B element (step s, column jj)
        function baddr(s, jj) result(addr)
            integer, intent(in) :: s, jj
            character(:), allocatable :: addr
            character(len=4), parameter :: base(0:2) = ['%rdx', '%r13', '%r14']
            if (spec%b_strided) then
                select case (mod(jj, 3))
                case (0)
                    addr = itoa(8*s)//'('//base(jj/3)//')'
                case (1)
                    addr = itoa(8*s)//'('//base(jj/3)//',%r12,1)'
                case default
                    addr = itoa(8*s)//'('//base(jj/3)//',%r12,2)'
                end select
            else
                addr = itoa(8*spec%nr*s + 8*jj)//'(%rdx)'
            end if
        end function baddr

        function reg(i) result(name)
            integer, intent(in) :: i
            character(:), allocatable :: name
            name = r//itoa(i)
        end function reg

        subroutine steps(t, count)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: count
            integer :: s, vv, jj, aoff, boff
            do s = 0, count - 1
                aoff = merge(0, 8*spec%mr*s, spec%a_strided)
                boff = 8*spec%nr*s
                if (spec%prefetch_a > 0 .and. .not. spec%a_strided) &
                    call asm(t, 'prefetcht0 '//itoa(spec%prefetch_a + aoff)//'(%rsi)')
                do vv = 0, mv - 1
                    call asm(t, 'vmovupd '//itoa(aoff + bytes*vv)//'(%rsi), '//reg(nacc + vv))
                end do
                do jj = 0, spec%nr - 1
                    if (wide .and. mv == 1) then
                        do vv = 0, mv - 1
                            call asm(t, 'vfmadd231pd '//baddr(s, jj)//'{1to8}, '// &
                                reg(nacc + vv)//', '//reg(vv + mv*jj))
                        end do
                    else
                        ! one broadcast per B element, reused for the mv A vectors; AVX-512
                        ! with mv = 1 broadcasts from memory inside the FMA instead
                        call asm(t, 'vbroadcastsd '//baddr(s, jj)//', '//reg(nacc + mv))
                        do vv = 0, mv - 1
                            call asm(t, 'vfmadd231pd '//reg(nacc + mv)//', '//reg(nacc + vv)//', '// &
                                reg(vv + mv*jj))
                        end do
                    end if
                end do
                if (spec%a_strided) call asm(t, 'add %rdi, %rsi')
            end do
        end subroutine steps

    end function x86_body

    ! aarch64: x0 = k, x1 = a, x2 = b, x3 = acc. Accumulator (v, j) is
    ! v(v + mv*j); A occupies the next mv registers, B the following nr/2,
    ! and each FMA multiplies by one lane of B.
    function neon_body(spec) result(text)
        type(tile_kernel_spec_t), intent(in) :: spec
        character(:), allocatable :: text, lab
        integer :: mv, nacc, nb, v, j, first, cnt

        mv = spec%mr/2
        nacc = mv*spec%nr
        nb = spec%nr/2
        lab = spec%name
        text = ''
        ! AAPCS64: the low halves of v8-v15 (d8-d15) are callee-saved
        call asm(text, 'stp d8, d9, [sp, #-64]!')
        call asm(text, 'stp d10, d11, [sp, #16]')
        call asm(text, 'stp d12, d13, [sp, #32]')
        call asm(text, 'stp d14, d15, [sp, #48]')
        if (spec%b_strided) then
            ! (k, a, b, ldb, c, ldc) in x0..x5: one pointer per column of B in
            ! x12..x17, each moving 8 bytes per step
            call asm(text, 'lsl x6, x3, #3')
            call asm(text, 'mov x12, x2')
            do j = 1, spec%nr - 1
                call asm(text, 'add x'//itoa(12 + j)//', x'//itoa(11 + j)//', x6')
            end do
            call asm(text, 'mov x3, x4')
            call asm(text, 'mov x4, x5')
        end if
        if (spec%a_strided) then
            ! (k, a, lda, b, c, ldc) in x0..x5 to the registers below; x14 =
            ! lda bytes minus the mr doubles a step's loads advance
            call asm(text, 'lsl x12, x2, #3')
            call asm(text, 'sub x14, x12, #'//itoa(8*spec%mr))
            call asm(text, 'mov x2, x3')
            call asm(text, 'mov x3, x4')
            call asm(text, 'mov x4, x5')
        end if
        do v = 0, nacc - 1
            call asm(text, 'movi v'//itoa(v)//'.2d, #0')
        end do
        call asm(text, 'mov x10, #'//itoa(spec%k_unroll))
        call asm(text, 'udiv x9, x0, x10')
        call asm(text, 'msub x11, x9, x10, x0')
        call asm(text, 'cbz x9, .L'//lab//'_tail')
        call label(text, '.L'//lab//'_main')
        call steps(text, spec%k_unroll)
        call asm(text, 'subs x9, x9, #1')
        call asm(text, 'b.ne .L'//lab//'_main')
        call label(text, '.L'//lab//'_tail')
        call asm(text, 'cbz x11, .L'//lab//'_store')
        call label(text, '.L'//lab//'_one')
        call steps(text, 1)
        call asm(text, 'subs x11, x11, #1')
        call asm(text, 'b.ne .L'//lab//'_one')
        call label(text, '.L'//lab//'_store')
        if (spec%update_c) then
            ! C(:, j) += tile(:, j): column pointer x5, ldc in bytes in x4; the
            ! A registers, free after the loop, hold each piece of C
            call asm(text, 'lsl x4, x4, #3')
            call asm(text, 'mov x5, x3')
            do j = 0, spec%nr - 1
                call asm(text, 'mov x6, x5')
                first = mv*j
                cnt = mv
                do while (cnt > 0)
                    call asm(text, 'ld1 '//reglist(nacc, min(cnt, 4))//', [x6]')
                    do v = 0, min(cnt, 4) - 1
                        call asm(text, 'fadd v'//itoa(first + v)//'.2d, v'//itoa(first + v)//'.2d, v'// &
                            itoa(nacc + v)//'.2d')
                    end do
                    call asm(text, 'st1 '//reglist(first, min(cnt, 4))//', [x6], #'//itoa(16*min(cnt, 4)))
                    first = first + min(cnt, 4)
                    cnt = cnt - min(cnt, 4)
                end do
                if (j < spec%nr - 1) call asm(text, 'add x5, x5, x4')
            end do
        else
            ! column j is registers mv*j .. mv*j+mv-1, contiguous in acc
            do j = 0, spec%nr - 1
                first = mv*j
                cnt = mv
                do while (cnt > 0)
                    call asm(text, 'st1 '//reglist(first, min(cnt, 4))//', [x3], #'//itoa(16*min(cnt, 4)))
                    first = first + min(cnt, 4)
                    cnt = cnt - min(cnt, 4)
                end do
            end do
        end if
        call asm(text, 'ldp d10, d11, [sp, #16]')
        call asm(text, 'ldp d12, d13, [sp, #32]')
        call asm(text, 'ldp d14, d15, [sp, #48]')
        call asm(text, 'ldp d8, d9, [sp], #64')
        call asm(text, 'ret')

    contains

        function reglist(first_reg, count) result(list)
            integer, intent(in) :: first_reg, count
            character(:), allocatable :: list
            integer :: i
            list = '{'
            do i = 0, count - 1
                if (i > 0) list = list//', '
                list = list//'v'//itoa(first_reg + i)//'.2d'
            end do
            list = list//'}'
        end function reglist

        subroutine steps(t, count)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: count
            integer :: s, vv, jj, f, c
            do s = 0, count - 1
                f = nacc
                c = mv
                do while (c > 0)
                    call asm(t, 'ld1 '//reglist(f, min(c, 4))//', [x1], #'//itoa(16*min(c, 4)))
                    f = f + min(c, 4)
                    c = c - min(c, 4)
                end do
                if (spec%a_strided) call asm(t, 'add x1, x1, x14')
                if (spec%b_strided) then
                    do jj = 0, spec%nr - 1
                        call asm(t, 'ld1 {v'//itoa(nacc + mv + jj/2)//'.d}['//itoa(mod(jj, 2))//'], [x'// &
                            itoa(12 + jj)//'], #8')
                    end do
                else
                    f = nacc + mv
                    c = nb
                    do while (c > 0)
                        call asm(t, 'ld1 '//reglist(f, min(c, 4))//', [x2], #'//itoa(16*min(c, 4)))
                        f = f + min(c, 4)
                        c = c - min(c, 4)
                    end do
                end if
                if (spec%prefetch_a > 0) &
                    call asm(t, 'prfm pldl1keep, [x1, #'//itoa(spec%prefetch_a)//']')
                do jj = 0, spec%nr - 1
                    do vv = 0, mv - 1
                        call asm(t, 'fmla v'//itoa(vv + mv*jj)//'.2d, v'//itoa(nacc + vv)// &
                            '.2d, v'//itoa(nacc + mv + jj/2)//'.d['//itoa(mod(jj, 2))//']')
                    end do
                end do
            end do
        end subroutine steps

    end function neon_body

    ! Apple AMX (M1 to M3) is undocumented; the encodings follow
    ! github.com/corsix/amx. An instruction is the word 0x201000 + 32 op + n
    ! with its 64-bit operand in xn; enable and disable are op 17 with
    ! immediate 0 and 1 after three nops. fp64 outer product (op 10): Z row
    ! 8 j + z, element i, += X(i) Y(j) for X and Y offsets in bytes in
    ! operand bits 10-18 and 0-8 and z in bits 20-25. The tile has
    ! (mr/8)(nr/8) <= 8 accumulators z = ib + (mr/8) jb, holding C(8 ib + i,
    ! 8 jb + j) in Z row 8 j + z; one column of C is consecutive rows, so it
    ! moves in pairs of rows. C is loaded into Z (acc is zeroed first),
    ! updated and stored back. A and B are loaded into X and Y in pairs of
    ! 64-byte registers; no address needs alignment (measured on M3: 32x16
    ! at k = 512 runs at 404 GFLOP/s on one core, unaligned C included).
    ! Each call enables and disables AMX, which costs nothing measurable.
    ! Every operand is built before the loop: the load operands in the high
    ! bits of the pointers, the outer-product operands in registers (x19 and
    ! up saved on the stack); rebuilding them per step cost a quarter of the
    ! speed (305 instead of 404 GFLOP/s).
    function amx_body(spec) result(text)
        type(tile_kernel_spec_t), intent(in) :: spec
        character(:), allocatable :: text, lab
        integer, parameter :: LDX = 0, LDY = 1, LDZ = 4, STZ = 5, FMA64 = 10
        ! registers for load pointers and outer-product operands beyond x1, x2
        integer, parameter :: pool(16) = [5, 8, 12, 13, 14, 15, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28]
        integer :: mb, nb, nx, ny, used, nsave, i, q, ib, jb
        integer, allocatable :: xptr(:), xoff(:), xflag(:), yptr(:), yoff(:), yflag(:), freg(:)
        logical :: pair
        mb = spec%mr/8
        nb = spec%nr/8
        lab = spec%name
        text = ''
        ! loads per step: pairs of registers from even ones
        allocate (xptr(0), xoff(0), xflag(0), yptr(0), yoff(0), yflag(0))
        q = 0
        do while (q < mb)
            pair = q + 1 < mb .and. mod(q, 2) == 0
            xoff = [xoff, 64*q]
            xflag = [xflag, 256*q + merge(16384, 0, pair)]
            q = q + merge(2, 1, pair)
        end do
        q = 0
        do while (q < nb)
            pair = q + 1 < nb .and. mod(q, 2) == 0
            yoff = [yoff, 64*q]
            yflag = [yflag, 256*q + merge(16384, 0, pair)]
            q = q + merge(2, 1, pair)
        end do
        nx = size(xoff)
        ny = size(yoff)
        used = 0
        xptr = [1, (pool(used + i), i = 1, nx - 1)]
        used = used + nx - 1
        yptr = [2, (pool(used + i), i = 1, ny - 1)]
        used = used + ny - 1
        freg = [(pool(used + i), i = 1, mb*nb)]
        used = used + mb*nb
        nsave = count(pool(1:used) >= 19)
        nsave = nsave + mod(nsave, 2)
        if (nsave > 0) then
            call asm(text, 'sub sp, sp, #'//itoa(8*nsave))
            do i = 0, nsave - 1, 2
                call asm(text, 'stp x'//itoa(19 + i)//', x'//itoa(20 + i)//', [sp, #'//itoa(8*i)//']')
            end do
        end if
        if (spec%a_strided) then
            ! (k, a, lda, b, c, ldc) to a = x1, b = x2, c = x3, ldc = x4; lda bytes in x6
            call asm(text, 'lsl x6, x2, #3')
            call asm(text, 'mov x2, x3')
            call asm(text, 'mov x3, x4')
            call asm(text, 'mov x4, x5')
        end if
        if (spec%update_c) then
            call asm(text, 'lsl x4, x4, #3')
        else
            call asm(text, 'movi v0.2d, #0')
            call asm(text, 'mov x16, x3')
            call asm(text, 'mov x17, #'//itoa(spec%mr*spec%nr/8))
            call label(text, '.L'//lab//'_zero')
            call asm(text, 'stp q0, q0, [x16], #32')
            call asm(text, 'stp q0, q0, [x16], #32')
            call asm(text, 'subs x17, x17, #1')
            call asm(text, 'b.ne .L'//lab//'_zero')
            call asm(text, 'mov x4, #'//itoa(8*spec%mr))
        end if
        ! the load operands live in the pointers: register index and pair
        ! flag in bits 56-62, which addressing ignores
        do i = nx, 1, -1
            call tag(text, xptr(i), 1, xoff(i), xflag(i))
        end do
        do i = ny, 1, -1
            call tag(text, yptr(i), 2, yoff(i), yflag(i))
        end do
        do jb = 0, nb - 1
            do ib = 0, mb - 1
                call constant(text, freg(1 + ib + mb*jb), 64*jb + 65536*ib + 1048576*(ib + mb*jb))
            end do
        end do
        call enable(text, 0)
        call z_transfer(text, LDZ)
        call asm(text, 'mov x10, #'//itoa(spec%k_unroll))
        call asm(text, 'udiv x9, x0, x10')
        call asm(text, 'msub x11, x9, x10, x0')
        call asm(text, 'cbz x9, .L'//lab//'_tail')
        call label(text, '.L'//lab//'_main')
        call steps(text, spec%k_unroll)
        call asm(text, 'subs x9, x9, #1')
        call asm(text, 'b.ne .L'//lab//'_main')
        call label(text, '.L'//lab//'_tail')
        call asm(text, 'cbz x11, .L'//lab//'_store')
        call label(text, '.L'//lab//'_one')
        call steps(text, 1)
        call asm(text, 'subs x11, x11, #1')
        call asm(text, 'b.ne .L'//lab//'_one')
        call label(text, '.L'//lab//'_store')
        call z_transfer(text, STZ)
        call enable(text, 1)
        if (nsave > 0) then
            do i = 0, nsave - 1, 2
                call asm(text, 'ldp x'//itoa(19 + i)//', x'//itoa(20 + i)//', [sp, #'//itoa(8*i)//']')
            end do
            call asm(text, 'add sp, sp, #'//itoa(8*nsave))
        end if
        call asm(text, 'ret')

    contains

        function word(op, xreg) result(w)
            integer, intent(in) :: op, xreg
            character(:), allocatable :: w
            character(len=16) :: buffer
            write (buffer, '(z0)') 2101248 + 32*op + xreg
            w = '.word 0x'//trim(buffer)
        end function word

        subroutine enable(t, off)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: off
            call asm(t, 'nop')
            call asm(t, 'nop')
            call asm(t, 'nop')
            call asm(t, word(17, off))
        end subroutine enable

        ! xr = x(base) + offset with flags << 48 in its top bits (x17 scratch)
        subroutine tag(t, xr, base, offset, flags)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: xr, base, offset, flags
            if (offset > 0) call asm(t, 'add x'//itoa(xr)//', x'//itoa(base)//', #'//itoa(offset))
            call asm(t, 'movz x17, #'//itoa(flags)//', lsl #48')
            call asm(t, 'orr x'//itoa(xr)//', x'//itoa(merge(base, xr, offset == 0))//', x17')
        end subroutine tag

        subroutine constant(t, xr, value)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: xr, value
            call asm(t, 'movz x'//itoa(xr)//', #'//itoa(mod(value, 65536)))
            if (value >= 65536) call asm(t, 'movk x'//itoa(xr)//', #'//itoa(value/65536)//', lsl #16')
        end subroutine constant

        ! every column of the tile between C (x3, ldc bytes in x4) and Z
        subroutine z_transfer(t, op)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: op
            integer :: j, b
            logical :: zpair
            call asm(t, 'mov x7, x3')
            do j = 0, spec%nr - 1
                b = 0
                do while (b < mb)
                    zpair = b + 1 < mb .and. mod(b, 2) == 0
                    if (b > 0) then
                        call asm(t, 'add x16, x7, #'//itoa(64*b))
                    else
                        call asm(t, 'mov x16, x7')
                    end if
                    call asm(t, 'movz x17, #'//itoa(256*(8*mod(j, 8) + b + mb*(j/8)) + &
                        merge(16384, 0, zpair))//', lsl #48')
                    call asm(t, 'orr x16, x16, x17')
                    call asm(t, word(op, 16))
                    b = b + merge(2, 1, zpair)
                end do
                if (j < spec%nr - 1) call asm(t, 'add x7, x7, x4')
            end do
        end subroutine z_transfer

        subroutine steps(t, count)
            character(:), allocatable, intent(inout) :: t
            integer, intent(in) :: count
            integer :: st, r
            do st = 1, count
                do r = 1, nx
                    call asm(t, word(LDX, xptr(r)))
                end do
                do r = 1, ny
                    call asm(t, word(LDY, yptr(r)))
                end do
                do r = 1, nx
                    if (spec%a_strided) then
                        call asm(t, 'add x'//itoa(xptr(r))//', x'//itoa(xptr(r))//', x6')
                    else
                        call asm(t, 'add x'//itoa(xptr(r))//', x'//itoa(xptr(r))//', #'//itoa(8*spec%mr))
                    end if
                end do
                do r = 1, ny
                    call asm(t, 'add x'//itoa(yptr(r))//', x'//itoa(yptr(r))//', #'//itoa(8*spec%nr))
                end do
                do r = 1, mb*nb
                    call asm(t, word(FMA64, freg(r)))
                end do
            end do
        end subroutine steps

    end function amx_body

    ! Local labels are written .Lname in the emitters and spelled through
    ! FG_L, because ELF assemblers want .L and Mach-O assemblers L.
    subroutine asm(text, instruction)
        character(:), allocatable, intent(inout) :: text
        character(*), intent(in) :: instruction
        text = text//'    "'//local_labels(instruction)//'\n"'//nl
    end subroutine asm

    subroutine label(text, name)
        character(:), allocatable, intent(inout) :: text
        character(*), intent(in) :: name
        text = text//'    "'//local_labels(name)//':\n"'//nl
    end subroutine label

    function local_labels(instruction) result(out)
        character(*), intent(in) :: instruction
        character(:), allocatable :: out
        integer :: i
        out = ''
        i = 1
        do while (i <= len(instruction))
            if (i < len(instruction)) then
                if (instruction(i:i + 1) == '.L') then
                    out = out//'" FG_L "'
                    i = i + 2
                    cycle
                end if
            end if
            out = out//instruction(i:i)
            i = i + 1
        end do
    end function local_labels

    subroutine add(text, line)
        character(:), allocatable, intent(inout) :: text
        character(*), intent(in) :: line
        text = text//line//nl
    end subroutine add

    pure function itoa(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(len=24) :: buffer
        write (buffer, '(i0)') i
        s = trim(buffer)
    end function itoa

end module fortgen_tile_kernel
