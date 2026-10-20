program generate_precision_fixture
    use fortgen_kernel_ir, only: kernel_ir_t
    use fortgen_kernel_emit, only: kernel_emit_spec_t
    use fortgen_ir_text, only: read_kernel_ir_file
    use fortgen_precision_emit, only: emit_cpu_precision_variants
    use fortgen_string, only: str_t, chars
    implicit none
    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source
    character(1024) :: input, output
    character(:), allocatable :: message
    integer :: unit
    logical :: ok
    if (command_argument_count() == 0) stop
    call get_command_argument(1, input)
    call get_command_argument(2, output)
    call read_kernel_ir_file(trim(input), ir, spec, ok, message)
    if (.not. ok) then
        print *, message
        error stop 1
    end if
    source = emit_cpu_precision_variants(ir, spec, ok, message)
    if (.not. ok) then
        print *, message
        error stop 2
    end if
    open(newunit=unit, file=trim(output), status='replace')
    write(unit,'(a)') chars(source)
    close(unit)
end program
