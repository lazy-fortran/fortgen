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
    call small_power_policy()

    call check_long_statements_continue()

contains

    subroutine check_long_statements_continue()
        !! A leaf with many arguments and a long sum must stay within the
        !! column limit and still be accepted by a Fortran compiler.
        integer, parameter :: n = 40
        type(kernel_ir_t) :: wide
        type(kernel_emit_spec_t) :: wide_spec
        type(str_t) :: wide_source
        character(:), allocatable :: text, why
        character(16) :: name
        integer :: k, start, stop, body, unit, status

        call kernel_ir_allocate(wide, n + 1, n, 1)
        allocate(wide_spec%args(n), wide_spec%outputs(1))
        do k = 1, n
            write(name, "(a,i0)") "argument_", k
            wide%nodes(k)%operation = IR_SYMBOL
            wide%nodes(k)%name = str(trim(name))
            wide%nodes(k)%first_operand = 1
            wide%operands(k) = k
            wide_spec%args(k) = str(trim(name))
        end do
        wide%nodes(n + 1)%operation = IR_ADD
        wide%nodes(n + 1)%first_operand = 1
        wide%nodes(n + 1)%n_operands = n
        wide%outputs(1) = n + 1
        wide_spec%name = str("wide_sum_kernel")
        wide_spec%outputs = [str("total")]
        wide_spec%temp_prefix = str("t")
        wide_spec%target = TARGET_FORTRAN_CPU
        wide_spec%producer = str("fortgen")
        wide_spec%generator = str("test_kernel_ir")
        wide_spec%regenerate_command = str("fpm test")

        wide_source = emit_fortran_kernel_ir(wide, wide_spec, ok, message)
        call require(ok, message)
        text = chars(wide_source)
        call require(index(text, " &"//new_line("a")) > 0, &
            "long statement was not continued")
        start = 1
        do while (start <= len(text))
            stop = index(text(start:), new_line("a"))
            if (stop == 0) stop = len(text) - start + 2
            stop = start + stop - 1
            body = verify(text(start:stop - 1), " ")
            if (body > 0) then
                if (text(start + body - 1:start + body - 1) /= "!") then
                    why = "line exceeds 88 columns: "//text(start:stop - 1)
                    call require(stop - start <= 88, why)
                end if
            end if
            start = stop + 1
        end do

        open(newunit=unit, file="fortgen-wide.f90", status="replace", &
            action="write")
        write(unit, "(a)") text
        close(unit)
        call execute_command_line("gfortran -std=f2018 -fsyntax-only " // &
            "-Werror=line-truncation fortgen-wide.f90", exitstat=status)
        call delete_file("fortgen-wide.f90")
        ! 127: no gfortran on this host; the column check above still ran.
        if (status /= 127) call require(status == 0, &
            "continued leaf does not compile")
    end subroutine check_long_statements_continue

    subroutine small_power_policy()
        type(kernel_ir_t) :: powers
        type(kernel_emit_spec_t) :: power_spec
        type(str_t) :: text
        real(kind(1.0d0)), parameter :: exponents(*) = [-1.0d0, -2.0d0, -3.0d0, &
            -4.0d0, -1.0000000000000002d0, -0.5d0, 1.0d100]
        integer :: i

        call kernel_ir_allocate(powers, 3, 2, 1)
        powers%nodes(1)%operation = IR_SYMBOL
        powers%nodes(1)%name = str("x")
        powers%nodes(2)%operation = IR_LITERAL
        powers%nodes(3)%operation = IR_POW
        powers%nodes(3)%first_operand = 1
        powers%nodes(3)%n_operands = 2
        powers%operands = [1, 2]
        powers%outputs = [3]
        power_spec%name = str("power_leaf")
        power_spec%args = [str("x")]
        power_spec%outputs = [str("value")]
        do i = 1, size(exponents)
            powers%nodes(2)%value = exponents(i)
            text = emit_cuda_device_ir(powers, power_spec, ok, message)
            call require(ok, message)
            ! The emitter's public source policy must recognize exact integers,
            ! not rounded neighbors, and honor the configured magnitude limit.
            call require((index(chars(text), "pow(") == 0) .eqv. (i <= 3), &
                "small-power exactness/limit changed")
        end do
        powers%nodes(2)%value = -1.0d0
        power_spec%policy%small_power_limit = 0
        text = emit_cuda_device_ir(powers, power_spec, ok, message)
        call require(ok, message)
        call require(index(chars(text), "pow(") > 0, "disabled policy expanded reciprocal")
    end subroutine small_power_policy

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
