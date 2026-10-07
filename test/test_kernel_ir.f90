program test_kernel_ir
    use fortgen_kernel_ir, only: kernel_ir_t, kernel_ir_allocate, &
        IR_LITERAL, IR_SYMBOL, IR_ADD, IR_POW, IR_FUNCTION
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, TARGET_FORTRAN_CPU, TARGET_CUDA
    use fortgen_ir_text, only: write_kernel_ir_file, read_kernel_ir_file
    use fortgen_string, only: str, str_t, chars
    implicit none

    type(kernel_ir_t) :: ir, replay
    type(kernel_emit_spec_t) :: spec, replay_spec
    type(str_t) :: source, replay_source, cuda_source
    character(:), allocatable :: message
    logical :: ok

    call kernel_ir_allocate(ir, 7, 7, 1)

    ir%nodes(1)%operation = IR_SYMBOL
    ir%nodes(1)%name = str("x")
    ir%nodes(1)%first_operand = 1

    ir%nodes(2)%operation = IR_SYMBOL
    ir%nodes(2)%name = str("y")
    ir%nodes(2)%first_operand = 1

    ir%nodes(3)%operation = IR_ADD
    ir%nodes(3)%first_operand = 1
    ir%nodes(3)%n_operands = 2
    ir%operands(1:2) = [1, 2]

    ir%nodes(4)%operation = IR_LITERAL
    ir%nodes(4)%value = 2.0d0
    ir%nodes(4)%first_operand = 3

    ir%nodes(5)%operation = IR_POW
    ir%nodes(5)%first_operand = 3
    ir%nodes(5)%n_operands = 2
    ir%operands(3:4) = [3, 4]

    ir%nodes(6)%operation = IR_FUNCTION
    ir%nodes(6)%name = str("sin")
    ir%nodes(6)%first_operand = 5
    ir%nodes(6)%n_operands = 1
    ir%operands(5) = 3

    ir%nodes(7)%operation = IR_ADD
    ir%nodes(7)%first_operand = 6
    ir%nodes(7)%n_operands = 2
    ir%operands(6:7) = [5, 6]
    ir%outputs(1) = 7

    allocate(spec%args(2), spec%outputs(1))
    spec%name = str("demo_kernel")
    spec%args = [str("x"), str("y")]
    spec%outputs = [str("result")]
    spec%temp_prefix = str("t")
    spec%target = TARGET_FORTRAN_CPU
    spec%producer = str("fortgen")
    spec%generator = str("test_kernel_ir")
    spec%regenerate_command = str("fpm test")

    source = emit_fortran_kernel_ir(ir, spec, ok, message)
    call require(ok, message)
    call require(index(chars(source), "subroutine demo_kernel") > 0, "missing subroutine")
    call require(index(chars(source), "sin(") > 0, "missing sin")
    call require(index(chars(source), "result = ") > 0, "missing output assignment")

    call write_kernel_ir_file("fortgen-test.fgir", ir, spec, ok, message)
    call require(ok, message)
    call read_kernel_ir_file("fortgen-test.fgir", replay, replay_spec, ok, message)
    call require(ok, message)
    replay_spec%generator = spec%generator
    replay_spec%regenerate_command = spec%regenerate_command
    replay_source = emit_fortran_kernel_ir(replay, replay_spec, ok, message)
    call require(ok, message)
    call require(chars(source) == chars(replay_source), "IR round-trip changed emitted source")

    replay_spec%target = TARGET_CUDA
    cuda_source = emit_cuda_device_ir(replay, replay_spec, ok, message)
    call require(ok, message)
    call require(index(chars(cuda_source), "__device__") > 0, "missing CUDA device marker")
    call require(index(chars(cuda_source), "pow(") > 0 .or. &
        index(chars(cuda_source), "*") > 0, "missing CUDA arithmetic")

    call delete_file("fortgen-test.fgir")

contains

    subroutine require(condition, why)
        logical, intent(in) :: condition
        character(*), intent(in) :: why
        if (.not. condition) then
            write(*, "(a)") "FAIL: "//trim(why)
            error stop 1
        end if
    end subroutine require

    subroutine delete_file(path)
        character(*), intent(in) :: path
        integer :: unit, ios
        open(newunit=unit, file=path, status="old", iostat=ios)
        if (ios == 0) close(unit, status="delete")
    end subroutine delete_file
end program test_kernel_ir
