program test_python_emitter
    !! Structure and refusal checks of the Python backend. Executed values
    !! are checked separately by test_python_kernel.py.
    use fortgen_ir_text, only: read_kernel_ir_file
    use fortgen_kernel_ir, only: kernel_ir_t
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, emit_python_kernel_ir, TARGET_DEFAULT, TARGET_CUDA, &
        PRECISION_REAL32
    use fortgen_string, only: str, str_t, chars
    implicit none

    type(kernel_ir_t) :: ir, renamed
    type(kernel_emit_spec_t) :: spec, changed
    type(str_t) :: source
    character(:), allocatable :: message, fixture
    character(len=4096) :: argument
    logical :: ok

    ! fpm runs tests from the project root and passes no argument.
    fixture = "test/python_kernel.fgir"
    if (command_argument_count() > 0) then
        call get_command_argument(1, argument)
        fixture = trim(argument)
    end if
    call read_kernel_ir_file(fixture, ir, spec, ok, message)
    call require(ok, message)
    spec%target = TARGET_DEFAULT

    source = emit_python_kernel_ir(ir, spec, ok, message)
    call require(ok, message)
    call require(index(chars(source), "def python_kernel(k, n, iota, sigma, z):") > 0, &
        "missing def line")
    call require(index(chars(source), "1j * ") > 0, "imaginary unit is not 1j")
    call require(index(chars(source), "numpy.pi") > 0, "missing numpy.pi")
    call require(index(chars(source), "numpy.arctan2(z, k)") > 0, "missing arctan2")
    call require(index(chars(source), "numpy.maximum(numpy.maximum(z, k), n)") > 0, &
        "n-ary Max is not nested")
    call require(index(chars(source), " ** 7)") > 0, "integer power is not exact")
    call require(index(chars(source), "    import numpy") > 0, "missing import")
    call require(index(chars(source), "    return (t15, t26, t28, t33)") > 0, &
        "outputs are not returned in order")

    changed = spec
    changed%docstring = str("Gauge weight.")
    source = emit_python_kernel_ir(ir, changed, ok, message)
    call require(ok, message)
    call require(index(chars(source), '    """Gauge weight."""') > 0, "missing docstring")
    changed%docstring = str('ends """ early')
    source = emit_python_kernel_ir(ir, changed, ok, message)
    call require(.not. ok .and. index(message, "docstring") > 0, "quote accepted")

    changed = spec
    changed%precision = PRECISION_REAL32
    source = emit_python_kernel_ir(ir, changed, ok, message)
    call require(.not. ok .and. index(message, "real64") > 0, "real32 accepted")

    changed = spec
    changed%target = TARGET_CUDA
    source = emit_python_kernel_ir(ir, changed, ok, message)
    call require(.not. ok, "device target accepted")

    changed = spec
    changed%args(1) = str("lambda")
    renamed = ir
    renamed%nodes(1)%name = str("lambda")
    source = emit_python_kernel_ir(renamed, changed, ok, message)
    call require(.not. ok .and. index(message, "reserved in Python: lambda") > 0, &
        "Python keyword accepted")

    renamed = ir
    renamed%nodes(20)%name = str("erf")
    source = emit_python_kernel_ir(renamed, spec, ok, message)
    call require(.not. ok .and. index(message, "unsupported Python function erf") > 0, &
        "function without a NumPy ufunc accepted")

    source = emit_fortran_kernel_ir(ir, spec, ok, message)
    call require(.not. ok .and. index(message, "Fortran constant i") > 0, &
        "Fortran accepted the imaginary unit")
    changed = spec
    changed%target = TARGET_CUDA
    source = emit_cuda_device_ir(ir, changed, ok, message)
    call require(.not. ok .and. index(message, "CUDA constant i") > 0, &
        "CUDA accepted the imaginary unit")
    write(*, "(a)") "test_python_emitter: all checks passed"

contains

    subroutine require(condition, why)
        logical, intent(in) :: condition
        character(*), intent(in) :: why
        if (.not. condition) then
            write(*, "(a)") "FAIL: "//trim(why)
            error stop 1
        end if
    end subroutine require
end program test_python_emitter
