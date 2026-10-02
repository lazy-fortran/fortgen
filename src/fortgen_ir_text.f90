module fortgen_ir_text
    !! Stable text interchange for the scalar kernel IR.
    !!
    !! The format is intentionally line-oriented and whitespace-separated so it
    !! is trivial to produce from Python and trivial to parse in Fortran.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortgen_kernel_ir, only: kernel_ir_t, kernel_ir_allocate, &
        IR_LITERAL, IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, IR_FUNCTION
    use fortgen_kernel_emit, only: kernel_emit_spec_t, TARGET_FORTRAN_CPU
    use fortgen_kernel_target, only: target_from_name
    use fortgen_precision, only: PRECISION_REAL64, precision_from_name
    use fortgen_string, only: str, chars
    implicit none
    private

    public :: read_kernel_ir_file, write_kernel_ir_file

contains

    subroutine read_kernel_ir_file(path, ir, spec, ok, message)
        character(*), intent(in) :: path
        type(kernel_ir_t), intent(inout) :: ir
        type(kernel_emit_spec_t), intent(out) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer :: unit, ios, n_nodes, n_operands, n_args, n_outputs
        integer :: idx, nops, i, j, node_cursor, operand_cursor, arg_cursor, out_cursor
        integer :: root, parsed_target, parsed_precision
        integer, allocatable :: tmp_ops(:)
        real(real64) :: value
        character(len=4096) :: line
        character(len=128) :: tag, kind, token
        character(len=128) :: kernel_name, target_name_text, precision_name_text
        logical :: saw_header

        ok = .false.
        message = ""
        kernel_name = ""
        target_name_text = "fortran_cpu"
        precision_name_text = "real64"
        saw_header = .false.
        n_nodes = 0
        n_operands = 0
        n_args = 0
        n_outputs = 0

        open(newunit=unit, file=path, status="old", action="read", iostat=ios)
        if (ios /= 0) then
            message = "FortGen IR: cannot open input file"
            return
        end if

        do
            read(unit, "(a)", iostat=ios) line
            if (ios < 0) exit
            if (ios > 0) then
                message = "FortGen IR: read error"
                close(unit)
                return
            end if
            if (len_trim(line) == 0 .or. line(1:1) == "#") cycle
            if (.not. saw_header) then
                if (trim(line) /= "fortgen.kernel_ir.v1") then
                    message = "FortGen IR: unsupported or missing header"
                    close(unit)
                    return
                end if
                saw_header = .true.
                cycle
            end if

            tag = ""
            read(line, *, iostat=ios) tag
            if (ios /= 0) cycle
            select case (trim(tag))
            case ("kernel")
                read(line, *, iostat=ios) tag, kernel_name
            case ("precision")
                read(line, *, iostat=ios) tag, precision_name_text
            case ("target")
                read(line, *, iostat=ios) tag, target_name_text
            case ("arg")
                n_args = n_args + 1
            case ("out")
                n_outputs = n_outputs + 1
            case ("node")
                idx = 0
                kind = ""
                read(line, *, iostat=ios) tag, idx, kind
                if (ios /= 0 .or. idx /= n_nodes + 1) then
                    message = "FortGen IR: node indices must be contiguous and 1-based"
                    close(unit)
                    return
                end if
                select case (trim(kind))
                case ("literal", "symbol", "constant")
                    nops = 0
                case ("add", "mul", "pow")
                    read(line, *, iostat=ios) tag, idx, kind, nops
                case ("function")
                    read(line, *, iostat=ios) tag, idx, kind, token, nops
                case default
                    ios = 1
                end select
                if (ios /= 0 .or. nops < 0) then
                    message = "FortGen IR: malformed node"
                    close(unit)
                    return
                end if
                n_nodes = n_nodes + 1
                n_operands = n_operands + nops
            case ("end")
                exit
            case default
                message = "FortGen IR: unknown record "//trim(tag)
                close(unit)
                return
            end select
            if (ios /= 0) then
                message = "FortGen IR: malformed record"
                close(unit)
                return
            end if
        end do
        if (.not. saw_header .or. len_trim(kernel_name) == 0) then
            message = "FortGen IR: kernel name is missing"
            close(unit)
            return
        end if

        parsed_target = target_from_name(trim(target_name_text))
        parsed_precision = precision_from_name(trim(precision_name_text))
        if (parsed_target == -999 .or. parsed_precision == 0) then
            message = "FortGen IR: invalid target or precision"
            close(unit)
            return
        end if

        call kernel_ir_allocate(ir, n_nodes, n_operands, n_outputs)
        allocate(spec%args(n_args))
        allocate(spec%outputs(n_outputs))
        spec%name = str(trim(kernel_name))
        spec%temp_prefix = str("t")
        spec%target = parsed_target
        spec%precision = parsed_precision
        spec%producer = str("fortgen")
        spec%generator = str("fortgen-codegen")
        spec%regenerate_command = str("fortgen-codegen")
        arg_cursor = 0
        out_cursor = 0
        node_cursor = 0
        operand_cursor = 0

        rewind(unit)
        read(unit, "(a)", iostat=ios) line
        do
            read(unit, "(a)", iostat=ios) line
            if (ios < 0) exit
            if (ios > 0) then
                message = "FortGen IR: read error in second pass"
                close(unit)
                return
            end if
            if (len_trim(line) == 0 .or. line(1:1) == "#") cycle
            tag = ""
            read(line, *, iostat=ios) tag
            if (ios /= 0) cycle
            select case (trim(tag))
            case ("arg")
                token = ""
                read(line, *, iostat=ios) tag, token
                arg_cursor = arg_cursor + 1
                spec%args(arg_cursor) = str(trim(token))
            case ("out")
                token = ""
                root = 0
                read(line, *, iostat=ios) tag, token, root
                out_cursor = out_cursor + 1
                spec%outputs(out_cursor) = str(trim(token))
                ir%outputs(out_cursor) = root
            case ("node")
                idx = 0
                kind = ""
                read(line, *, iostat=ios) tag, idx, kind
                if (ios /= 0) then
                    message = "FortGen IR: malformed node in second pass"
                    close(unit)
                    return
                end if
                node_cursor = node_cursor + 1
                ir%nodes(node_cursor)%first_operand = operand_cursor + 1
                select case (trim(kind))
                case ("literal")
                    read(line, *, iostat=ios) tag, idx, kind, value
                    ir%nodes(node_cursor)%operation = IR_LITERAL
                    ir%nodes(node_cursor)%value = value
                    nops = 0
                case ("symbol")
                    read(line, *, iostat=ios) tag, idx, kind, token
                    ir%nodes(node_cursor)%operation = IR_SYMBOL
                    ir%nodes(node_cursor)%name = str(trim(token))
                    nops = 0
                case ("constant")
                    read(line, *, iostat=ios) tag, idx, kind, token
                    ir%nodes(node_cursor)%operation = IR_CONSTANT
                    ir%nodes(node_cursor)%name = str(trim(token))
                    nops = 0
                case ("add", "mul", "pow")
                    read(line, *, iostat=ios) tag, idx, kind, nops
                    if (nops > 0) then
                        allocate(tmp_ops(nops))
                        read(line, *, iostat=ios) tag, idx, kind, nops, &
                            (tmp_ops(j), j = 1, nops)
                    end if
                    select case (trim(kind))
                    case ("add"); ir%nodes(node_cursor)%operation = IR_ADD
                    case ("mul"); ir%nodes(node_cursor)%operation = IR_MUL
                    case ("pow"); ir%nodes(node_cursor)%operation = IR_POW
                    end select
                case ("function")
                    token = ""
                    read(line, *, iostat=ios) tag, idx, kind, token, nops
                    if (nops > 0) then
                        allocate(tmp_ops(nops))
                        read(line, *, iostat=ios) tag, idx, kind, token, nops, &
                            (tmp_ops(j), j = 1, nops)
                    end if
                    ir%nodes(node_cursor)%operation = IR_FUNCTION
                    ir%nodes(node_cursor)%name = str(trim(token))
                case default
                    ios = 1
                end select
                if (ios /= 0) then
                    if (allocated(tmp_ops)) deallocate(tmp_ops)
                    message = "FortGen IR: malformed node payload"
                    close(unit)
                    return
                end if
                ir%nodes(node_cursor)%n_operands = nops
                if (nops > 0) then
                    ir%operands(operand_cursor + 1:operand_cursor + nops) = tmp_ops
                    operand_cursor = operand_cursor + nops
                    deallocate(tmp_ops)
                end if
            case ("end")
                exit
            end select
        end do
        close(unit)

        ir%n_nodes = node_cursor
        ir%n_operands = operand_cursor
        if (arg_cursor /= n_args .or. out_cursor /= n_outputs .or. &
            node_cursor /= n_nodes .or. operand_cursor /= n_operands) then
            message = "FortGen IR: second-pass counts disagree"
            return
        end if
        do i = 1, size(ir%outputs)
            if (ir%outputs(i) < 1 .or. ir%outputs(i) > ir%n_nodes) then
                message = "FortGen IR: output root is invalid"
                return
            end if
        end do
        ok = .true.
    end subroutine read_kernel_ir_file

    subroutine write_kernel_ir_file(path, ir, spec, ok, message)
        character(*), intent(in) :: path
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: unit, ios, i, j, first, last
        character(:), allocatable :: kind

        ok = .false.
        message = ""
        open(newunit=unit, file=path, status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            message = "FortGen IR: cannot open output file"
            return
        end if
        write(unit, "(a)") "fortgen.kernel_ir.v1"
        write(unit, "(a,1x,a)") "kernel", chars(spec%name)
        write(unit, "(a,1x,a)") "precision", precision_text(spec%precision)
        write(unit, "(a,1x,a)") "target", target_text(spec%target)
        do i = 1, size(spec%args)
            write(unit, "(a,1x,a)") "arg", chars(spec%args(i))
        end do
        do i = 1, size(spec%outputs)
            write(unit, "(a,1x,a,1x,i0)") "out", chars(spec%outputs(i)), ir%outputs(i)
        end do
        do i = 1, ir%n_nodes
            select case (ir%nodes(i)%operation)
            case (IR_LITERAL)
                write(unit, "(a,1x,i0,1x,a,1x,es25.16e3)") &
                    "node", i, "literal", ir%nodes(i)%value
            case (IR_SYMBOL)
                write(unit, "(a,1x,i0,1x,a,1x,a)") &
                    "node", i, "symbol", chars(ir%nodes(i)%name)
            case (IR_CONSTANT)
                write(unit, "(a,1x,i0,1x,a,1x,a)") &
                    "node", i, "constant", chars(ir%nodes(i)%name)
            case (IR_ADD, IR_MUL, IR_POW, IR_FUNCTION)
                select case (ir%nodes(i)%operation)
                case (IR_ADD); kind = "add"
                case (IR_MUL); kind = "mul"
                case (IR_POW); kind = "pow"
                case (IR_FUNCTION); kind = "function"
                end select
                first = ir%nodes(i)%first_operand
                last = first + ir%nodes(i)%n_operands - 1
                if (ir%nodes(i)%operation == IR_FUNCTION) then
                    write(unit, "(a,1x,i0,1x,a,1x,a,1x,i0)", advance="no") &
                        "node", i, kind, chars(ir%nodes(i)%name), ir%nodes(i)%n_operands
                else
                    write(unit, "(a,1x,i0,1x,a,1x,i0)", advance="no") &
                        "node", i, kind, ir%nodes(i)%n_operands
                end if
                do j = first, last
                    write(unit, "(1x,i0)", advance="no") ir%operands(j)
                end do
                write(unit, *)
            end select
        end do
        write(unit, "(a)") "end"
        close(unit)
        ok = .true.
    end subroutine write_kernel_ir_file

    pure function precision_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        select case(value)
        case (1); text = "real64"
        case (2); text = "real32"
        case (3); text = "mixed"
        case default; text = "invalid"
        end select
    end function precision_text

    pure function target_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        select case(value)
        case (-1); text = "default"
        case (0); text = "fortran_cpu"
        case (1); text = "fortran_openmp_target"
        case (2); text = "fortran_openacc"
        case (3); text = "fortran_openmp_target+fortran_openacc"
        case (4); text = "cuda"
        case default; text = "invalid"
        end select
    end function target_text

end module fortgen_ir_text
