program generate_tensor
    use fortgen_kernel_ir, only: kernel_ir_t, kernel_ir_allocate, IR_SYMBOL, &
                                 IR_ADD, IR_MUL
    use fortgen_kernel_emit, only: kernel_emit_spec_t, PRECISION_REAL32, &
                                   PRECISION_REAL64
    use fortgen_tensor_emit, only: tensor_kernel_spec_t, emit_tensor_fortran, &
                                   emit_tensor_cuda
    use fortgen_string, only: str, str_t, chars
    implicit none
    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: leaf
    type(tensor_kernel_spec_t) :: t
    type(str_t) :: source
    character(2048) :: path, arg, mode
    character(:), allocatable :: message
    integer :: d, p, extra, tables, bits, nc, ns, i, j, node, op, unit
    logical :: ok

    call get_command_argument(1, mode)
    call get_command_argument(2, path)
    call get_command_argument(3, arg)
    read (arg, *) d
    call get_command_argument(4, arg)
    read (arg, *) p
    call get_command_argument(5, arg)
    read (arg, *) extra
    call get_command_argument(6, arg)
    read (arg, *) tables
    call get_command_argument(7, arg)
    read (arg, *) bits
    call get_command_argument(8, arg)
    t%name = str(trim(arg))
    t%dimension = d
    t%nloc = p + 1
    t%nq = p + 1 + extra
    t%cell_tables = tables == 1
    t%precision = PRECISION_REAL64
    if (bits == 32) t%precision = PRECISION_REAL32
    nc = d + 1
    t%ncoeff = nc*nc
    ns = nc + t%ncoeff
    allocate (leaf%args(ns), leaf%outputs(nc))
    call kernel_ir_allocate(ir, ns + nc*nc + nc, 3*nc*nc, nc)
    do i = 1, ns
        leaf%args(i) = str('x'//num(i))
        ir%nodes(i)%operation = IR_SYMBOL
        ir%nodes(i)%name = leaf%args(i)
    end do
    node = ns
    op = 0
    do i = 1, nc
        leaf%outputs(i) = str('f'//num(i))
        do j = 1, nc
            node = node + 1
            ir%nodes(node)%operation = IR_MUL
            ir%nodes(node)%first_operand = op + 1
            ir%nodes(node)%n_operands = 2
            ir%operands(op + 1:op + 2) = [j, nc + (i - 1)*nc + j]
            op = op + 2
        end do
        node = node + 1
        ir%nodes(node)%operation = IR_ADD
        ir%nodes(node)%first_operand = op + 1
        ir%nodes(node)%n_operands = nc
        ir%operands(op + 1:op + nc) = [(node - nc + j - 1, j=1, nc)]
        op = op + nc
        ir%outputs(i) = node
    end do
    ! Exercise refusal before opening any output file.
    t%dimension = 1
    source = emit_tensor_fortran(ir, leaf, t, ok, message)
    if (ok .or. len(chars(source)) /= 0) error stop 'invalid dimension accepted'
    t%dimension = d
    t%nq = t%nloc - 1
    source = emit_tensor_cuda(ir, leaf, t, ok, message)
    if (ok .or. len(chars(source)) /= 0) error stop 'invalid quadrature accepted'
    t%nq = p + 1 + extra
    leaf%name = str('pointwise')
    if (trim(mode) == 'fortran') then
        source = emit_tensor_fortran(ir, leaf, t, ok, message)
    else if (trim(mode) == 'cuda') then
        source = emit_tensor_cuda(ir, leaf, t, ok, message)
    else
        error stop 'expected fortran or cuda'
    end if
    if (.not. ok) then
        print *, message
        error stop 'tensor emission failed'
    end if
    open (newunit=unit, file=trim(path), status='replace')
    write (unit, '(a)') chars(source)
    close (unit)
contains
    function num(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(32) :: tmp
        write (tmp, '(i0)') i
        s = trim(tmp)
    end function
end program
