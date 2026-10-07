program fortgen_codegen
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fortgen_ir_text, only: read_kernel_ir_file
    use fortgen_kernel_ir, only: kernel_ir_t
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, TARGET_CUDA
    use fortgen_string, only: str_t, chars
    implicit none

    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source
    character(len=32) :: backend
    character(len=2048) :: input_path, output_path
    character(:), allocatable :: message
    integer :: narg, unit, ios
    logical :: ok

    narg = command_argument_count()
    if (narg < 2 .or. narg > 3) then
        write(error_unit, "(a)") "usage: fortgen-codegen fortran|cuda INPUT [OUTPUT]"
        error stop 2
    end if
    call get_command_argument(1, backend)
    call get_command_argument(2, input_path)
    output_path = ""
    if (narg == 3) call get_command_argument(3, output_path)

    call read_kernel_ir_file(trim(input_path), ir, spec, ok, message)
    if (.not. ok) then
        write(error_unit, "(a)") trim(message)
        error stop 1
    end if

    select case (trim(backend))
    case ("fortran")
        source = emit_fortran_kernel_ir(ir, spec, ok, message)
    case ("cuda")
        spec%target = TARGET_CUDA
        source = emit_cuda_device_ir(ir, spec, ok, message)
    case default
        write(error_unit, "(a)") "fortgen-codegen: backend must be fortran or cuda"
        error stop 2
    end select
    if (.not. ok) then
        write(error_unit, "(a)") trim(message)
        error stop 1
    end if

    if (len_trim(output_path) == 0) then
        write(output_unit, "(a)", advance="no") chars(source)
    else
        open(newunit=unit, file=trim(output_path), status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            write(error_unit, "(a)") "fortgen-codegen: cannot open output file"
            error stop 1
        end if
        write(unit, "(a)", advance="no") chars(source)
        close(unit)
    end if
end program fortgen_codegen
