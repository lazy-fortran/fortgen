program test_select_emitter
    !! Spelling, refusal and text round-trip checks of relational selections
    !! and CUDA extrema. Executed values are checked by the compiled, device
    !! and Python oracles of the same fixture.
    use fortgen_ir_text, only: read_kernel_ir_file, write_kernel_ir_file
    use fortgen_kernel_ir, only: kernel_ir_t
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, emit_cuda_batch_ir, emit_python_kernel_ir, &
        emit_metal_device_ir, PRECISION_REAL32
    use fortgen_string, only: str, str_t, chars
    implicit none

    type(kernel_ir_t) :: ir, changed_ir, replay
    type(kernel_emit_spec_t) :: spec, changed, replay_spec
    type(str_t) :: source, replay_source
    character(:), allocatable :: message, fixture
    character(len=4096) :: argument
    logical :: ok

    ! fpm runs tests from the project root and passes no argument.
    fixture = "test/select_kernel.fgir"
    if (command_argument_count() > 0) then
        call get_command_argument(1, argument)
        fixture = trim(argument)
    end if
    call read_kernel_ir_file(fixture, ir, spec, ok, message)
    call require(ok, message)

    source = emit_fortran_kernel_ir(ir, spec, ok, message)
    call require(ok, message)
    call require(index(chars(source), "merge(x, 1.0000000000000000E+000_real64, "// &
        "x /= 0.0000000000000000E+000_real64)") > 0, "Fortran select_ne spelling")
    call require(index(chars(source), "merge(t11, t12, x < y)") > 0, &
        "Fortran select_lt spelling")
    call require(index(chars(source), "merge(t11, t12, x <= y)") > 0, &
        "Fortran select_le spelling")
    call require(index(chars(source), "x == y)") > 0, "Fortran select_eq spelling")

    call write_kernel_ir_file("fortgen-select-test.fgir", ir, spec, ok, message)
    call require(ok, message)
    call read_kernel_ir_file("fortgen-select-test.fgir", replay, replay_spec, ok, &
        message)
    call require(ok, message)
    replay_source = emit_fortran_kernel_ir(replay, replay_spec, ok, message)
    call require(ok, message)
    call require(chars(source) == chars(replay_source), &
        "IR round-trip changed emitted selections")
    call delete_file("fortgen-select-test.fgir")

    source = emit_cuda_device_ir(ir, spec, ok, message)
    call require(ok, message)
    call require(index(chars(source), "((x < y) ? t11 : t12)") > 0, &
        "CUDA select_lt spelling")
    call require(index(chars(source), "((x <= y) ? t11 : t12)") > 0, &
        "CUDA select_le spelling")
    call require(index(chars(source), "((x == y) ? ") > 0, &
        "CUDA select_eq spelling")
    call require(index(chars(source), "((x != 0.0000000000000000E+000) ? ") > 0, &
        "CUDA select_ne spelling")
    call require(index(chars(source), "fmin(fmin(x, y), 2.5000000000000000E-001)") > 0, &
        "CUDA n-ary Min is not nested fmin")
    call require(index(chars(source), "fmax(fmax(x, y), 2.5000000000000000E-001)") > 0, &
        "CUDA n-ary Max is not nested fmax")
    source = emit_cuda_batch_ir(ir, spec, ok, message)
    call require(ok, message)
    call require(index(chars(source), "select_kernel_batch(") > 0, "CUDA batch")
    call require(index(chars(source), " < ") > 0 .and. &
        index(chars(source), " <= ") > 0 .and. &
        index(chars(source), " == ") > 0 .and. &
        index(chars(source), " != ") > 0, "CUDA batch relation spellings")

    changed = spec
    changed%precision = PRECISION_REAL32
    source = emit_cuda_device_ir(ir, changed, ok, message)
    call require(ok, message)
    call require(index(chars(source), "fminf(fminf(x, y), ") > 0, "real32 CUDA Min")
    call require(index(chars(source), "fmaxf(fmaxf(x, y), ") > 0, "real32 CUDA Max")
    call require(index(chars(source), "fmin(") == 0 .and. &
        index(chars(source), "fmax(") == 0, "real32 CUDA kept a binary64 extremum")

    source = emit_python_kernel_ir(ir, spec, ok, message)
    call require(ok, message)
    call require(index(chars(source), "numpy.where(x < y, t11, t12)") > 0, &
        "Python select_lt spelling")
    call require(index(chars(source), "numpy.where(x <= y, t11, t12)") > 0, &
        "Python select_le spelling")
    call require(index(chars(source), "numpy.where(x == y, ") > 0, &
        "Python select_eq spelling")
    call require(index(chars(source), "numpy.where(x != 0.0") > 0, &
        "Python select_ne spelling")

    source = emit_metal_device_ir(ir, changed, ok, message)
    call require(.not. ok .and. index(message, "unsupported Metal function select_ne") > 0, &
        "Metal accepted an unverified selection")

    changed_ir = ir
    changed_ir%nodes(13)%n_operands = 3
    source = emit_fortran_kernel_ir(changed_ir, spec, ok, message)
    call require(.not. ok .and. index(message, "wrong arity for function select_lt") > 0, &
        "three-operand selection accepted")

    changed = spec
    changed%args(2) = str("merge")
    changed_ir = ir
    changed_ir%nodes(2)%name = str("merge")
    source = emit_fortran_kernel_ir(changed_ir, changed, ok, message)
    call require(.not. ok .and. index(message, "merge intrinsic") > 0, &
        "argument named merge accepted beside a Fortran selection")
    source = emit_cuda_device_ir(changed_ir, changed, ok, message)
    call require(ok, "the Fortran merge restriction leaked into CUDA: "//message)
    write(*, "(a)") "test_select_emitter: all checks passed"

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
end program test_select_emitter
