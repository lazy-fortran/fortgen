program generate_temporary_name_fixture
    use, intrinsic :: iso_fortran_env, only: real64
    use fortgen_kernel_ir, only: kernel_ir_t, kernel_ir_allocate, &
        IR_SYMBOL, IR_LITERAL, IR_ADD, IR_MUL
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, TARGET_CUDA
    use fortgen_string, only: str_t, str, chars
    implicit none
    type(kernel_ir_t) :: small, large
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source
    character(2048) :: path
    character(:), allocatable :: message
    integer :: unit, k
    logical :: ok

    call kernel_ir_allocate(large, 10, 15, 1)
    large%nodes(1)%operation = IR_SYMBOL
    large%nodes(1)%name = str("x")
    large%nodes(2)%operation = IR_SYMBOL
    large%nodes(2)%name = str("y")
    do k = 3, 6
        large%nodes(k)%operation = IR_MUL
        large%nodes(k)%first_operand = 2*k - 5
        large%nodes(k)%n_operands = 2
    end do
    large%nodes(3)%operation = IR_ADD
    large%nodes(7)%operation = IR_LITERAL
    large%nodes(7)%value = 2.0_real64
    large%nodes(8)%operation = IR_MUL
    large%nodes(8)%first_operand = 9
    large%nodes(8)%n_operands = 2
    large%nodes(9)%operation = IR_ADD
    large%nodes(9)%first_operand = 11
    large%nodes(9)%n_operands = 3
    large%nodes(10)%operation = IR_ADD
    large%nodes(10)%first_operand = 14
    large%nodes(10)%n_operands = 2
    large%operands = [1, 2, 1, 1, 2, 2, 1, 2, 6, 7, 4, 5, 8, 9, 3]
    large%outputs = [10]
    call kernel_ir_allocate(small, 3, 2, 1)
    small%nodes = large%nodes(:3)
    small%operands = [1, 2]
    small%outputs = [3]
    spec%args = [str("x"), str("y")]
    spec%outputs = [str("value")]
    spec%name = str("temporary_name_kernel")

    ! Prefix63 plus the first compound node suffix is an invalid64-character
    ! Fortran identifier, even though the prefix itself is legal.
    spec%temp_prefix = str(repeat("t", 63))
    source = emit_fortran_kernel_ir(small, spec, ok, message)
    if (ok) error stop "accepted overlength one-digit temporary identifier"
    if (len(chars(source)) /= 0) error stop "failed emission returned source"
    ! The boundary changes when node numbers gain a second decimal digit.
    spec%temp_prefix = str(repeat("t", 62))
    source = emit_fortran_kernel_ir(large, spec, ok, message)
    if (ok) error stop "accepted overlength two-digit temporary identifier"

    call get_command_argument(1, path)
    if (len_trim(path) == 0) error stop "missing generated fixture path"
    open(newunit=unit, file=trim(path), status="replace", action="write")
    source = emit_fortran_kernel_ir(small, spec, ok, message)
    if (.not. ok) error stop "rejected valid63-character one-digit temporary"
    write(unit, "(a)") chars(source)
    spec%name = str("temporary_name_kernel_large")
    spec%temp_prefix = str(repeat("t", 61))
    source = emit_fortran_kernel_ir(large, spec, ok, message)
    if (.not. ok) error stop "rejected valid63-character two-digit temporary"
    write(unit, "(a)") chars(source)
    close(unit)

    ! CUDA identifiers have no corresponding Fortran63-character limit.
    spec%target = TARGET_CUDA
    spec%temp_prefix = str(repeat("t", 63))
    source = emit_cuda_device_ir(large, spec, ok, message)
    if (.not. ok) error stop "Fortran temporary limit leaked into CUDA emitter"
end program generate_temporary_name_fixture
