program test_metal_emitter
    !! Structure, refusal, policy-knob and target round-trip checks of the
    !! Metal backend. Executed device values are checked separately by
    !! test/device/test_metal_kernel.mm, which compiles this same emission.
    use fortgen_ir_text, only: read_kernel_ir_file, write_kernel_ir_file
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, emit_python_kernel_ir, emit_metal_device_ir, &
        TARGET_DEFAULT, TARGET_FORTRAN_CPU, TARGET_FORTRAN_OPENMP_TARGET, &
        TARGET_CUDA, TARGET_METAL, PRECISION_REAL32, PRECISION_REAL64, &
        PRECISION_MIXED
    use fortgen_kernel_ir, only: kernel_ir_t, kernel_ir_allocate, IR_SYMBOL, &
        IR_FUNCTION
    use fortgen_kernel_target, only: target_is_valid, target_name, &
        target_from_name, target_directives
    use fortgen_string, only: str, str_t, chars
    implicit none

    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source, first
    character(:), allocatable :: fixture, message
    character(len=4096) :: argument
    logical :: ok

    fixture = "test/metal_kernel.fgir"
    if (command_argument_count() > 0) then
        call get_command_argument(1, argument)
        fixture = trim(argument)
    end if
    call read_kernel_ir_file(fixture, ir, spec, ok, message)
    call require(ok, "fixture: "//message)
    call require(spec%target == TARGET_METAL, "fixture target is not metal")
    call require(spec%precision == PRECISION_REAL32, "fixture is not real32")

    first = emit_metal_device_ir(ir, spec, ok, message)
    call require(ok, message)
    call emitted_shape(chars(first))
    call policy_knobs()
    call refusals()
    call reserved_names()
    call round_trip(first)
    write(*, "(a)") "test_metal_emitter: all checks passed"

contains

    !> The leaf must be MSL that CuMetal consumes as written: no CUDA
    !> decoration, no suffix-f libm, explicit thread output pointers.
    subroutine emitted_shape(text)
        character(*), intent(in) :: text

        call require(index(text, "#include <metal_stdlib>") > 0, &
            "missing metal_stdlib include")
        call require(index(text, "inline") > 0, "Metal leaf is not inline")
        call require(index(text, 'extern "C"') == 0, "CUDA linkage on Metal leaf")
        call require(index(text, "__device__") == 0, "CUDA attribute on Metal leaf")
        call require(index(text, &
            "void metal_reciprocal(const float x, const float dx, "// &
            "const float seed, thread float* value") > 0, "unexpected Metal signature")
        call require(index(text, "thread float* scaled") > 0, &
            "last output is not a thread pointer")
        call require(index(text, "metal::sin(") > 0, "sine is not in namespace metal")
        call require(index(text, "metal::max(metal::max(x, dx), seed)") > 0, &
            "n-ary Max is not nested into binary metal::max")
        call require(index(text, "metal::pow(") > 0, "general power is not metal::pow")
        call require(index(text, "metal::acos(-1.0f)") > 0, &
            "pi is not a Metal constant expression")
        call require(index(text, "E+000f") > 0, "literals are not real32")
        call require(index(text, "double") == 0, "double reached Metal output")
        call require(index(text, "acosf") == 0, "CUDA suffix-f math in Metal output")
        call require(index(text, "real32") == 0, "Fortran kind in Metal output")
        call require(index(text, "*value = ") > 0, "output 1 is not stored")
        call require(index(text, "*refined = ") > 0, "output 2 is not stored")
        call require(index(text, "*adjoint = ") > 0, "output 6 is not stored")
        call require(index(text, "*scaled = ") > 0, "output 10 is not stored")
        ! The refinement add inlines its single-use multiply child (node 11),
        ! which is what lets the Metal compiler contract it into an fma.
        call require(index(text, "t11") == 0, "FMA-shaped child was materialized")
    end subroutine emitted_shape

    !> Conservative arithmetic knobs keep their meaning for Metal.
    subroutine policy_knobs()
        type(kernel_emit_spec_t) :: tuned
        type(str_t) :: text

        tuned = spec
        tuned%policy%shape_fma = .false.
        text = emit_metal_device_ir(ir, tuned, ok, message)
        call require(ok, message)
        call require(index(chars(text), "t11") > 0, &
            "shape_fma off did not materialize the multiply")

        tuned = spec
        tuned%policy%small_power_limit = 0
        text = emit_metal_device_ir(ir, tuned, ok, message)
        call require(ok, message)
        call require(index(chars(text), &
            "metal::pow(x, (-1.0000000000000000E+000f))") > 0, &
            "small_power_limit 0 did not retain the general Metal power")
    end subroutine policy_knobs

    subroutine refusals()
        type(kernel_emit_spec_t) :: bad
        type(kernel_ir_t) :: altered

        bad = spec
        bad%precision = PRECISION_REAL64
        source = emit_metal_device_ir(ir, bad, ok, message)
        call require(.not. ok .and. index(message, "only real32") > 0, &
            "real64 accepted for Metal: "//message)
        call require(index(message, "software binary64") > 0, &
            "real64 refusal does not state that no binary64 leaf exists")

        bad = spec
        bad%precision = PRECISION_MIXED
        source = emit_metal_device_ir(ir, bad, ok, message)
        call require(.not. ok .and. index(message, "only real32") > 0, &
            "mixed accepted for Metal")

        bad = spec
        bad%target = TARGET_CUDA
        source = emit_metal_device_ir(ir, bad, ok, message)
        call require(.not. ok .and. index(message, "Metal emitter") > 0, &
            "CUDA target accepted by the Metal emitter")

        bad = spec
        bad%target = TARGET_FORTRAN_OPENMP_TARGET
        source = emit_metal_device_ir(ir, bad, ok, message)
        call require(.not. ok, "OpenMP target accepted by the Metal emitter")

        bad = spec
        bad%target = TARGET_DEFAULT
        source = emit_metal_device_ir(ir, bad, ok, message)
        call require(ok, "default target refused by the Metal emitter")

        bad = spec
        bad%target = TARGET_METAL
        source = emit_fortran_kernel_ir(ir, bad, ok, message)
        call require(.not. ok .and. index(message, "Metal target requires Metal") > 0, &
            "Fortran emitter accepted the Metal target")
        source = emit_cuda_device_ir(ir, bad, ok, message)
        call require(.not. ok, "CUDA emitter accepted the Metal target")
        source = emit_python_kernel_ir(ir, bad, ok, message)
        call require(.not. ok, "Python emitter accepted the Metal target")

        call unsupported_functions()

        altered = ir
        altered%nodes(8)%name = str("i")
        source = emit_metal_device_ir(altered, spec, ok, message)
        call require(.not. ok .and. index(message, "unsupported Metal constant i") > 0, &
            "Metal accepted the imaginary unit")
    end subroutine refusals

    subroutine reserved_names()
        type(kernel_emit_spec_t) :: bad
        type(kernel_ir_t) :: altered
        character(len=12), parameter :: names(8) = [character(len=12) :: &
            "thread", "device", "constant", "kernel", "float", "metal", "class", "float3"]
        integer :: j

        do j = 1, size(names)
            bad = spec
            bad%name = str(trim(names(j)))
            source = emit_metal_device_ir(ir, bad, ok, message)
            call require(.not. ok .and. index(message, "reserved in Metal") > 0, &
                "reserved Metal procedure accepted: "//trim(names(j)))
            bad = spec
            bad%outputs(1) = str(trim(names(j)))
            source = emit_metal_device_ir(ir, bad, ok, message)
            call require(.not. ok .and. index(message, "reserved in Metal") > 0, &
                "reserved Metal output accepted: "//trim(names(j)))
            bad = spec
            altered = ir
            bad%args(1) = str(trim(names(j)))
            altered%nodes(1)%name = bad%args(1)
            source = emit_metal_device_ir(altered, bad, ok, message)
            call require(.not. ok .and. index(message, "reserved in Metal") > 0, &
                "reserved Metal argument accepted: "//trim(names(j)))
        end do
        bad = spec
        bad%args(1) = str("THREAD")
        altered = ir
        altered%nodes(1)%name = str("thread")
        source = emit_metal_device_ir(altered, bad, ok, message)
        call require(.not. ok .and. index(message, "symbol name is reserved") > 0, &
            "reserved Metal symbol accepted through case-insensitive argument lookup")
        bad = spec
        bad%temp_prefix = str("t")
        bad%args(1) = str("t9")
        altered = ir
        altered%nodes(1)%name = bad%args(1)
        source = emit_metal_device_ir(altered, bad, ok, message)
        call require(.not. ok .and. index(message, "temporary collides") > 0, &
            "Metal temporary collision accepted")
    end subroutine reserved_names

    !> Every refused name was checked against the installed Metal toolchain,
    !> which declares none of them in namespace metal.
    subroutine unsupported_functions()
        type(kernel_ir_t) :: one
        type(kernel_emit_spec_t) :: one_spec
        character(len=12), parameter :: names(6) = [character(len=12) :: &
            "erf", "erfc", "gamma", "loggamma", "elliptic_k", "elliptic_e"]
        integer :: j

        do j = 1, size(names)
            call kernel_ir_allocate(one, 2, 1, 1)
            one%nodes(1)%operation = IR_SYMBOL
            one%nodes(1)%name = str("x")
            one%nodes(1)%first_operand = 1
            one%nodes(2)%operation = IR_FUNCTION
            one%nodes(2)%name = str(names(j))
            one%nodes(2)%first_operand = 1
            one%nodes(2)%n_operands = 1
            one%operands(1) = 1
            one%outputs(1) = 2
            allocate (one_spec%args(1), one_spec%outputs(1))
            one_spec%name = str("metal_probe")
            one_spec%args = [str("x")]
            one_spec%outputs = [str("y")]
            one_spec%temp_prefix = str("t")
            one_spec%target = TARGET_METAL
            one_spec%precision = PRECISION_REAL32
            one_spec%producer = str("fortgen")
            one_spec%generator = str("test_metal_emitter")
            source = emit_metal_device_ir(one, one_spec, ok, message)
            call require(.not. ok .and. &
                index(message, "unsupported Metal function "//trim(names(j))) > 0, &
                "Metal accepted "//trim(names(j)))
            deallocate (one_spec%args, one_spec%outputs)
        end do
    end subroutine unsupported_functions

    subroutine round_trip(expected)
        type(str_t), intent(in) :: expected
        type(kernel_ir_t) :: replay
        type(kernel_emit_spec_t) :: replay_spec
        type(str_t) :: text
        logical :: openmp, openacc
        character(:), allocatable :: path

        path = "fortgen-metal-roundtrip.fgir"
        call write_kernel_ir_file(path, ir, spec, ok, message)
        call require(ok, message)
        call read_kernel_ir_file(path, replay, replay_spec, ok, message)
        call require(ok, "read back: "//message)
        call require(replay_spec%target == TARGET_METAL, "metal target did not round-trip")
        call require(replay_spec%precision == PRECISION_REAL32, &
            "real32 did not round-trip")
        text = emit_metal_device_ir(replay, replay_spec, ok, message)
        call require(ok, message)
        call require(chars(text) == chars(expected), "replayed Metal differs")
        call delete_file(path)

        call require(TARGET_METAL == 5, "metal was not appended as identity 5")
        call require(TARGET_CUDA == 4 .and. TARGET_FORTRAN_CPU == 0 .and. &
            TARGET_DEFAULT == -1, "existing target identities were renumbered")
        call require(target_is_valid(TARGET_METAL), "metal target is invalid")
        call require(target_name(TARGET_METAL) == "metal", "metal target name")
        call require(target_from_name("metal") == TARGET_METAL, "metal name parse")
        call require(target_from_name("cuda") == TARGET_CUDA, "cuda name parse")
        call require(target_from_name("metall") == -999, "near-miss name accepted")
        call require(.not. target_is_valid(6), "unassigned target identity accepted")
        call target_directives(TARGET_METAL, .false., .false., openmp, openacc)
        call require(.not. openmp .and. .not. openacc, &
            "metal target emitted an offload directive")
    end subroutine round_trip

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

        open (newunit=unit, file=path, status="old", iostat=ios)
        if (ios == 0) close(unit, status="delete")
    end subroutine delete_file
end program test_metal_emitter
