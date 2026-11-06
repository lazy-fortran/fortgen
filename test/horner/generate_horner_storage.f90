program generate_horner_storage
    use fortgen_horner_emit, only: horner_kernel_spec_t, emit_horner_kernel
    use fortgen_kernel_ir, only: TYPE_REAL32, TYPE_REAL64, TYPE_UINT32
    use fortgen_derivative_set, only: partials
    use fortgen_kernel_target, only: TARGET_FORTRAN_CPU, TARGET_FORTRAN_OPENACC, &
        TARGET_CUDA
    use fortgen_string, only: str, str_t, chars
    implicit none
    type(horner_kernel_spec_t) :: spec, explicit
    type(str_t) :: source, original
    character(2048) :: mode, path
    character(:), allocatable :: message
    logical :: ok
    integer :: u, target, storage

    call get_command_argument(1, mode)
    call get_command_argument(2, path)
    if (trim(mode) /= 'cuda' .and. trim(mode) /= 'fortran') error stop 'mode'
    spec%name = str('horner_storage64')
    spec%derivatives = partials(3, 1)
    spec%degree = 5
    spec%nq = 3
    do target = TARGET_FORTRAN_CPU, TARGET_CUDA
        if (target /= TARGET_FORTRAN_CPU .and. &
            target /= TARGET_FORTRAN_OPENACC .and. target /= TARGET_CUDA) cycle
        spec%target = target
        original = emit_horner_kernel(spec, ok, message)
        if (.not. ok) error stop 'default refused'
        explicit = spec
        explicit%coefficient_type = TYPE_REAL64
        explicit%compute_type = TYPE_REAL64
        source = emit_horner_kernel(explicit, ok, message)
        if (.not. ok) error stop 'explicit binary64 refused'
        if (chars(source) /= chars(original)) error stop 'default source changed'
        explicit%coefficient_type = TYPE_UINT32
        source = emit_horner_kernel(explicit, ok, message)
        if (ok .or. len(chars(source)) /= 0) error stop 'integer storage accepted'
        explicit = spec
        explicit%compute_type = TYPE_REAL32
        source = emit_horner_kernel(explicit, ok, message)
        if (ok .or. len(chars(source)) /= 0) error stop 'binary32 compute accepted'
    end do
    spec%target = TARGET_FORTRAN_CPU
    if (trim(mode) == 'cuda') spec%target = TARGET_CUDA
    open (newunit=u, file=trim(path), status='replace', action='write')
    if (trim(mode) == 'fortran') then
        write (u, '(a)') 'module horner_storage_kernels'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
    end if
    do storage = 1, 2
        spec%coefficient_type = TYPE_REAL64
        spec%name = str('horner_storage64')
        if (storage == 2) then
            spec%coefficient_type = TYPE_REAL32
            spec%name = str('horner_storage32')
        end if
        source = emit_horner_kernel(spec, ok, message)
        if (.not. ok) error stop 'storage case refused'
        write (u, '(a)') chars(source)
    end do
    if (trim(mode) == 'fortran') write (u, '(a)') 'end module'
    close (u)
end program
