program test_tile_kernel
    ! Structure of the emitted tile kernels; test/tile_kernel_check.c runs them.
    use fortgen_tile_kernel
    implicit none
    type(tile_kernel_spec_t) :: s
    character(:), allocatable :: src, msg
    logical :: ok

    s%name = 'k'
    s%isa = ISA_X86_64_AVX2_FMA
    s%mr = 8
    s%nr = 8
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok .and. index(msg, 'registers') > 0, 'AVX2 8x8 needs 19 of 16 registers')
    s%nr = 6
    call require(tile_register_count(s) == 15, 'AVX2 8x6 register count')
    s%mr = 6
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok .and. index(msg, 'multiple') > 0, 'mr must fill whole vectors')
    s%mr = 8
    s%k_unroll = 4
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok, 'AVX2 8x6 emitted')
    ! 4 steps of 6 broadcasts and 12 FMAs, plus one step in the tail loop
    call require(count_of(src, 'vfmadd231pd') == 5*12, 'AVX2 FMA count')
    call require(count_of(src, 'vbroadcastsd') == 5*6, 'AVX2 one broadcast per B element')
    call require(count_of(src, 'vmovupd %ymm') == 12, 'AVX2 stores the whole tile')
    call require(index(src, 'vzeroupper') > 0, 'AVX2 leaves no dirty upper state')
    call require(index(src, '#define FG_L "L"') > 0 .and. index(src, '#define FG_L ".L"') > 0, &
        'local labels for Mach-O and ELF')
    call require(count_of(src, '".L') == 1, 'ELF local labels only through FG_L')
    call require(index(src, 'defined(__x86_64__)') > 0, 'x86 kernel compiled only on x86')
    call require(index(src, '", @function\n"') > 0 .and. count_of(src, 'FG_SIZE(') == 3, &
        'ELF type and size of the kernel symbol')

    s%update_c = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, 'vaddpd') == 12 .and. index(src, 'mov %r8, %r11') > 0, &
        'AVX2 C update: one add per accumulator, ldc kept out of the division')
    call require(index(src, 'double *c, int64_t ldc') > 0, 'C-update prototype')
    s%a_strided = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, 'add %rdi, %rsi') == 5 .and. index(src, 'shl $3, %rdi') > 0, &
        'strided A advances by lda per step')
    call require(index(src, 'int64_t lda') > 0, 'strided prototype')
    s%b_strided = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok, 'A and B cannot both be unpacked')
    s%a_strided = .false.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. index(src, 'push %r12') > 0 .and. index(src, 'pop %r12') > 0 .and. &
        count_of(src, ',%r12,') == 4*5 + 2, 'strided B: column groups by scaled ldb, callee-saved kept')
    s%b_strided = .false.
    s%update_c = .false.
    s%trsm = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, 'vfnmadd231pd') == 5*12 + 6*(4*2 + 4*1) .and. &
        count_of(src, 'vblendpd') == 8*6 .and. count_of(src, 'vpermpd') == 8*6 .and. &
        index(src, 'double *pb') > 0, 'AVX2 trsm: subtracting product, one solve step per element')
    call require(index(src, 'mov 24(%rsp), %r13') > 0 .and. index(src, 'pop %r12') > 0, &
        'trsm: seventh argument from the stack, callee-saved registers kept')
    s%update_c = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok .and. index(msg, 'trsm') > 0, 'trsm does not combine with a C update')
    s%update_c = .false.
    s%trsm = .false.

    s%isa = ISA_X86_64_AVX512
    s%mr = 24
    s%nr = 8
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. tile_register_count(s) == 28, 'AVX-512 24x8')
    call require(count_of(src, 'vbroadcastsd') == 5*8, 'AVX-512 tall tile broadcasts into a register')
    call require(index(src, '{1to8}') == 0, 'no memory broadcast in a tall tile')
    s%mr = 8
    s%nr = 24
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, '{1to8}') == 5*24, 'AVX-512 one-vector tile broadcasts in the FMA')

    s%isa = ISA_AARCH64_NEON
    s%mr = 8
    s%nr = 6
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. tile_register_count(s) == 31, 'NEON 8x6')
    call require(count_of(src, 'fmla') == 5*24, 'NEON FMA count')
    call require(index(src, 'stp d8, d9, [sp, #-64]!') > 0 .and. &
        index(src, 'ldp d8, d9, [sp], #64') > 0, 'NEON keeps d8-d15')
    s%update_c = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, 'fadd') == 24 .and. index(src, 'lsl x4, x4, #3') > 0, &
        'NEON C update through the free A registers')
    s%a_strided = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, 'add x1, x1, x14') == 5, 'NEON strided A')
    s%a_strided = .false.
    s%b_strided = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, '], #8') == 5*6, 'NEON strided B: one lane load per element')
    s%b_strided = .false.
    s%update_c = .false.
    s%nr = 5
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok, 'NEON loads B in pairs')

    ! AMX: 32x16 fills the 8 fp64 accumulators; per step two pair loads of A,
    ! one of B and 8 outer products, all operands in registers; C moves in
    ! pairs of Z rows
    s%isa = ISA_AARCH64_AMX
    s%mr = 32
    s%nr = 16
    s%k_unroll = 2
    s%prefetch_a = 0
    s%update_c = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. tile_register_count(s) == 8, 'AMX 32x16')
    call require(count_of(src, '.word 0x20114') + count_of(src, '.word 0x20115') == 3*8, &
        'AMX fp64 outer products in steps 0 and 1 and the tail')
    call require(count_of(src, '.word 0x201001') == 3 .and. count_of(src, '.word 0x201005') == 3 .and. &
        count_of(src, '.word 0x201022') == 3, 'AMX loads A and B in pairs through tagged pointers')
    call require(count_of(src, '.word 0x201090') == 16*2 .and. count_of(src, '.word 0x2010B0') == 16*2, &
        'AMX moves C in pairs of Z rows')
    call require(count_of(src, '.word 0x201220') == 1 .and. count_of(src, '.word 0x201221') == 1, &
        'AMX enabled and disabled once')
    call require(count_of(src, 'movk') == 7 .and. index(src, 'stp x19, x20') > 0 .and. &
        index(src, 'ldp x21, x22') > 0, 'AMX operands built once, x19-x22 kept')
    call require(index(src, 'defined(__APPLE__)') > 0 .and. index(src, 'ldc);') > 0, 'AMX only on Apple')
    s%update_c = .false.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. count_of(src, 'stp q0, q0') == 2 .and. index(src, 'mov x4, #256') > 0, &
        'AMX zeroes acc and moves it like C')
    s%mr = 64
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok .and. index(msg, 'registers') > 0, 'AMX 64x16 needs 16 of 8 accumulators')
    s%mr = 32
    s%nr = 12
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok, 'AMX nr in whole Y registers')
    s%nr = 16
    s%b_strided = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok, 'AMX cannot read B unpacked')
    s%b_strided = .false.

    s%isa = ISA_WASM32_SIMD128
    s%mr = 10
    s%nr = 6
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok .and. index(msg, 'registers') > 0, 'wasm 10x6 needs 36 of 32 locals')
    s%mr = 8
    s%nr = 4
    s%k_unroll = 4
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. tile_register_count(s) == 21, 'wasm 8x4')
    call require(index(src, '.functype k (i64, i32, i32, i32) -> ()') > 0, 'wasm signature (k, a, b, acc)')
    ! 4 steps in the main loop and one in the tail, 16 products each; no
    ! relaxed-SIMD fused multiply-add, which Safari's engine rejects
    call require(count_of(src, 'f64x2.mul') == 5*16 .and. count_of(src, 'f64x2.add') == 5*16, &
        'wasm multiply and add per product')
    call require(count_of(src, 'relaxed') == 0, 'wasm without relaxed SIMD')
    call require(count_of(src, 'v128.load64_splat') == 5*4, 'wasm one broadcast per B element')
    call require(count_of(src, 'v128.store') == 16, 'wasm stores the whole tile')
    call require(index(src, 'defined(__wasm_simd128__)') > 0 .and. count_of(src, 'FG_SIZE(') == 2 .and. &
        index(src, '.p2align') == 0, 'wasm kernel only with SIMD128, without size or alignment directives')
    call require(count_of(src, 'end_function') == 1, 'wasm function closed')
    s%update_c = .true.
    s%b_strided = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. index(src, '.functype k (i64, i32, i32, i64, i32, i64) -> ()') > 0 .and. &
        count_of(src, 'f64x2.add') == 5*16 + 16, 'wasm unpacked B added to C')
    s%b_strided = .false.
    s%a_strided = .true.
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(ok .and. index(src, '.functype k (i64, i32, i64, i32, i32, i64) -> ()') > 0, &
        'wasm unpacked A')
    s%a_strided = .false.
    s%update_c = .false.

    s%isa = 99
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok .and. index(msg, 'unknown') > 0, 'unknown instruction set rejected')
    s%isa = ISA_X86_64_AVX2_FMA
    s%mr = 8
    s%nr = 6
    s%k_unroll = 0
    src = emit_tile_kernel_c_asm(s, ok, msg)
    call require(.not. ok, 'k unroll must be positive')
    print '(a)', 'tile kernel structure: all checks passed'

contains

    subroutine require(condition, what)
        logical, intent(in) :: condition
        character(*), intent(in) :: what
        if (.not. condition) then
            print '(a)', 'FAILED: '//what
            error stop 1
        end if
    end subroutine require

    integer function count_of(text, pattern) result(n)
        character(*), intent(in) :: text, pattern
        integer :: at, i
        n = 0
        i = 1
        do
            at = index(text(i:), pattern)
            if (at == 0) exit
            n = n + 1
            i = i + at + len(pattern) - 1
        end do
    end function count_of

end program test_tile_kernel
