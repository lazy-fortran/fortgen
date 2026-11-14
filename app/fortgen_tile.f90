program fortgen_tile
    ! fortgen-tile ISA MR NR K_UNROLL PREFETCH NAME OUTPUT [update_c,a_strided]
    ! Writes one register-tile kernel as a C translation unit with the assembly.
    use fortgen_tile_kernel, only: tile_kernel_spec_t, emit_tile_kernel_c_asm, &
        ISA_X86_64_AVX2_FMA, ISA_X86_64_AVX512, ISA_AARCH64_NEON
    implicit none
    type(tile_kernel_spec_t) :: spec
    character(len=256) :: arg(8)
    character(:), allocatable :: source, message
    logical :: ok
    integer :: i, unit

    if (command_argument_count() < 7 .or. command_argument_count() > 8) then
        write (*, '(a)') 'usage: fortgen-tile ISA MR NR K_UNROLL PREFETCH NAME OUTPUT [update_c,a_strided]'
        write (*, '(a)') '  ISA: x86_64_avx2_fma, x86_64_avx512, aarch64_neon'
        error stop 2
    end if
    arg = ''
    do i = 1, command_argument_count()
        call get_command_argument(i, arg(i))
    end do
    select case (trim(arg(1)))
    case ('x86_64_avx2_fma')
        spec%isa = ISA_X86_64_AVX2_FMA
    case ('x86_64_avx512')
        spec%isa = ISA_X86_64_AVX512
    case ('aarch64_neon')
        spec%isa = ISA_AARCH64_NEON
    case default
        write (*, '(a)') 'fortgen-tile: unknown ISA '//trim(arg(1))
        error stop 2
    end select
    read (arg(2), *) spec%mr
    read (arg(3), *) spec%nr
    read (arg(4), *) spec%k_unroll
    read (arg(5), *) spec%prefetch_a
    spec%name = trim(arg(6))
    spec%update_c = index(arg(8), 'update_c') > 0
    spec%a_strided = index(arg(8), 'a_strided') > 0
    source = emit_tile_kernel_c_asm(spec, ok, message)
    if (.not. ok) then
        write (*, '(a)') 'fortgen-tile: '//message
        error stop 1
    end if
    open (newunit=unit, file=trim(arg(7)), status='replace', action='write', &
        access='stream', form='unformatted')
    write (unit) source
    close (unit)
end program fortgen_tile
