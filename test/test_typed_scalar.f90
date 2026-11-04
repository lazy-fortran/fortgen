program test_typed_scalar
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortgen_kernel_ir
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_cuda_device_ir, &
        emit_metal_device_ir, emit_fortran_kernel_ir, emit_python_kernel_ir, &
        emit_cuda_batch_ir, PRECISION_REAL32
    use fortgen_ir_text, only: write_kernel_ir_file
    use fortgen_string, only: str, str_t, chars
    implicit none
    type(kernel_ir_t) :: ir, bad
    type(kernel_emit_spec_t) :: spec, altered
    type(str_t) :: source
    character(:), allocatable :: message
    character(4096) :: path
    logical :: ok, exists
    integer :: count, used, x, y, t, z, upper, valid, safe, q, wide, prod
    integer :: lo, hi, total, carry, diff, borrow, large, signbit, maximum
    integer :: shifted, masked, merged, one, residual, cert, unit, mode, j
    integer :: product_hi, no_overflow, zero64
    integer :: root(17), out_type(17)

    call kernel_ir_allocate(ir, 80, 160, 17)
    count = 0; used = 0
    x = node(IR_SYMBOL, TYPE_UINT64, name='x')
    y = node(IR_SYMBOL, TYPE_UINT64, name='y')
    t = node(IR_SYMBOL, TYPE_REAL32, name='estimate')
    z = node(IR_LITERAL, TYPE_REAL32)
    upper = node(IR_LITERAL, TYPE_REAL32, value=65536.0_real64)
    valid = node(IR_GE, TYPE_BOOL, [t, z])
    one = node(IR_LT, TYPE_BOOL, [t, upper])
    valid = node(IR_AND, TYPE_BOOL, [valid, one])
    safe = node(IR_SELECT, TYPE_REAL32, [valid, t, z])
    q = node(IR_CAST, TYPE_UINT32, [safe])
    wide = node(IR_CAST, TYPE_UINT64, [q])
    prod = node(IR_MUL, TYPE_UINT64, [wide, y])
    residual = node(IR_SUB, TYPE_UINT64, [x, prod])
    cert = node(IR_LE, TYPE_BOOL, [prod, x])
    cert = node(IR_AND, TYPE_BOOL, [valid, cert])
    product_hi = node(IR_MULHI, TYPE_UINT64, [wide, y])
    zero64 = node(IR_LITERAL, TYPE_UINT64)
    no_overflow = node(IR_EQ, TYPE_BOOL, [product_hi, zero64])
    cert = node(IR_AND, TYPE_BOOL, [cert, no_overflow])
    lo = node(IR_MUL, TYPE_UINT64, [x, y])
    hi = node(IR_MULHI, TYPE_UINT64, [x, y])
    total = node(IR_ADD, TYPE_UINT64, [x, y])
    carry = node(IR_LT, TYPE_BOOL, [total, x])
    diff = node(IR_SUB, TYPE_UINT64, [x, y])
    borrow = node(IR_LT, TYPE_BOOL, [x, y])
    large = node(IR_LITERAL, TYPE_UINT64, high=2097152_int64, low=1_int64)
    signbit = node(IR_LITERAL, TYPE_UINT64, high=2147483648_int64)
    maximum = node(IR_LITERAL, TYPE_UINT64, &
        high=4294967295_int64, low=4294967295_int64)
    shifted = node(IR_SHR, TYPE_UINT64, [x], shift=32)
    masked = node(IR_BITAND, TYPE_UINT64, [x, signbit])
    merged = node(IR_BITOR, TYPE_UINT64, [x, signbit])
    root = [lo, hi, total, carry, diff, borrow, large, signbit, maximum, &
        shifted, masked, merged, q, valid, residual, cert, prod]
    ir%outputs = root
    ir%nodes = ir%nodes(:count)
    ir%operands = ir%operands(:used)
    ir%n_nodes = count; ir%n_operands = used
    spec%name = str('typed_digit')
    spec%args = [str('x'), str('y'), str('estimate')]
    spec%arg_types = [TYPE_UINT64, TYPE_UINT64, TYPE_REAL32]
    spec%outputs = [str('lo'), str('hi'), str('sum'), str('carry'), &
        str('difference'), str('borrow'), str('large'), str('signbit'), &
        str('maximum'), str('shifted'), str('masked'), str('merged'), &
        str('digit'), str('valid'), str('residual'), str('cert'), str('product')]
    do j = 1, size(root)
        out_type(j) = ir%nodes(root(j))%scalar_type
    end do
    spec%output_types = out_type
    spec%precision = PRECISION_REAL32
    spec%policy%small_power_limit = 0
    spec%policy%fold_exact_constants = .false.
    spec%policy%eliminate_constant_divisions = .false.
    spec%policy%shape_fma = .false.
    source = emit_cuda_device_ir(ir, spec, ok, message)
    call require(ok, message)
    call require(index(chars(source), '0x0020000000000001ull') > 0, 'inexact 2**53+1')
    call require(index(chars(source), '0x8000000000000000ull') > 0, 'inexact 2**63')
    call require(index(chars(source), '0xFFFFFFFFFFFFFFFFull') > 0, 'inexact max uint64')
    call refusals()
    do mode = 1, min(2, command_argument_count())
        call get_command_argument(mode, path)
        if (mode == 1) source = emit_cuda_device_ir(ir, spec, ok, message)
        if (mode == 2) source = emit_metal_device_ir(ir, spec, ok, message)
        call require(ok, message)
        open(newunit=unit, file=trim(path), status='replace')
        write(unit, '(a)') chars(source)
        close(unit)
    end do
    call check_uint32()
    print '(a)', 'typed scalar validation passed' 
contains
    integer function node(op, scalar, children, name, value, high, low, shift) result(id)
        integer, intent(in) :: op, scalar
        integer, intent(in), optional :: children(:), shift
        character(*), intent(in), optional :: name
        real(real64), intent(in), optional :: value
        integer(int64), intent(in), optional :: high, low
        count = count + 1
        id = count
        ir%nodes(id)%operation = op
        ir%nodes(id)%scalar_type = scalar
        ir%nodes(id)%first_operand = used + 1
        if (present(children)) then
            ir%nodes(id)%n_operands = size(children)
            ir%operands(used + 1:used + size(children)) = children
            used = used + size(children)
        end if
        if (present(name)) ir%nodes(id)%name = str(name)
        if (present(value)) ir%nodes(id)%value = value
        if (present(high)) ir%nodes(id)%uint_hi = high
        if (present(low)) ir%nodes(id)%uint_lo = low
        if (present(shift)) ir%nodes(id)%shift = shift
    end function node

    subroutine check_uint32()
        integer :: a, b, bit, sh, sub, add, mul, high, lt, eq, ne, gt, le, ge
        integer :: both, either, inverted, real_a, real_b, quotient, selected, truth, lie
        call kernel_ir_allocate(ir, 40, 80, 19)
        count = 0; used = 0
        a = node(IR_SYMBOL, TYPE_UINT32, name='a')
        b = node(IR_SYMBOL, TYPE_UINT32, name='b')
        bit = node(IR_LITERAL, TYPE_UINT32, low=2147483648_int64)
        sh = node(IR_SHL, TYPE_UINT32, [a], shift=1)
        sub = node(IR_SUB, TYPE_UINT32, [a, b])
        add = node(IR_ADD, TYPE_UINT32, [a, b])
        mul = node(IR_MUL, TYPE_UINT32, [a, b])
        high = node(IR_MULHI, TYPE_UINT32, [a, b])
        lt = node(IR_LT, TYPE_BOOL, [a, b])
        eq = node(IR_EQ, TYPE_BOOL, [a, b])
        ne = node(IR_NE, TYPE_BOOL, [a, b])
        gt = node(IR_GT, TYPE_BOOL, [a, b])
        le = node(IR_LE, TYPE_BOOL, [a, b])
        ge = node(IR_GE, TYPE_BOOL, [a, b])
        both = node(IR_AND, TYPE_BOOL, [ne, le])
        either = node(IR_OR, TYPE_BOOL, [lt, eq])
        truth = node(IR_LITERAL, TYPE_BOOL)
        ir%nodes(truth)%bool_value = .true.
        lie = node(IR_LITERAL, TYPE_BOOL)
        both = node(IR_AND, TYPE_BOOL, [both, truth])
        either = node(IR_OR, TYPE_BOOL, [either, lie])
        inverted = node(IR_NOT, TYPE_BOOL, [lt])
        real_a = node(IR_CAST, TYPE_REAL32, [a])
        real_b = node(IR_LITERAL, TYPE_REAL32, value=2.0_real64)
        quotient = node(IR_DIV, TYPE_REAL32, [real_a, real_b])
        selected = node(IR_SELECT, TYPE_UINT32, [lt, a, b])
        ir%outputs = [bit, sh, sub, add, mul, high, lt, eq, ne, gt, le, ge, &
            both, either, inverted, real_a, quotient, selected, a]
        ir%nodes = ir%nodes(:count)
        ir%operands = ir%operands(:used)
        ir%n_nodes = count; ir%n_operands = used
        spec%name = str('typed_u32')
        spec%args = [str('a'), str('b')]
        spec%arg_types = [TYPE_UINT32, TYPE_UINT32]
        spec%outputs = [str('bit'), str('shifted'), str('difference'), str('sum'), &
            str('product'), str('high'), str('lt'), str('eq'), str('ne'), str('gt'), &
            str('le'), str('ge'), str('both'), str('either'), str('inverted'), &
            str('real_a'), str('quotient'), str('selected'), str('identity')]
        spec%output_types = ir%nodes(ir%outputs)%scalar_type
        source = emit_cuda_device_ir(ir, spec, ok, message)
        call require(ok, message)
        do mode = 1, min(2, command_argument_count())
            call get_command_argument(mode, path)
            if (mode == 1) source = emit_cuda_device_ir(ir, spec, ok, message)
            if (mode == 2) source = emit_metal_device_ir(ir, spec, ok, message)
            call require(ok, message)
            open(newunit=unit, file=trim(path), status='old', position='append')
            write(unit, '(a)') chars(source)
            close(unit)
        end do
    end subroutine check_uint32

    subroutine refusals()
        character(32) :: preserved
        integer :: k
        bad = ir
        bad%nodes(large)%uint_hi = 4294967296_int64
        call reject(bad, 'literal range')
        bad = ir
        bad%nodes(large)%uint_hi = -1
        call reject(bad, 'negative unsigned digit')
        bad = ir
        bad%nodes(large)%scalar_type = TYPE_UINT32
        call reject(bad, 'uint32 width')
        bad = ir
        bad%nodes(shifted)%shift = 64
        call reject(bad, 'shift width')
        bad%nodes(shifted)%shift = -1
        call reject(bad, 'negative shift')
        bad = ir
        bad%nodes(lo)%scalar_type = TYPE_UINT32
        call reject(bad, 'mixed unsigned widths')
        bad = ir
        bad%nodes(safe)%scalar_type = TYPE_UINT32
        call reject(bad, 'select type mismatch')
        bad = ir
        bad%nodes(q)%scalar_type = TYPE_BOOL
        call reject(bad, 'unsupported cast')
        bad = ir
        bad%nodes(x)%scalar_type = TYPE_DEFAULT
        call reject(bad, 'mixed typed and implicit graph')
        bad = ir
        bad%nodes(lo)%n_operands = -1
        call reject(bad, 'negative arity')
        altered = spec
        altered%arg_types(1) = TYPE_UINT32
        source = emit_cuda_device_ir(ir, altered, ok, message)
        call require(.not. ok, 'accepted incorrect symbol type')
        altered = spec
        altered%output_types(1) = TYPE_UINT32
        source = emit_cuda_device_ir(ir, altered, ok, message)
        call require(.not. ok, 'accepted incorrect output type')
        altered = spec
        altered%policy%shape_fma = .true.
        source = emit_cuda_device_ir(ir, altered, ok, message)
        call require(.not. ok, 'accepted non-strict policy')
        do k = 1, 3
            altered = spec
            if (k == 1) altered%policy%fold_exact_constants = .true.
            if (k == 2) altered%policy%eliminate_constant_divisions = .true.
            if (k == 3) altered%policy%small_power_limit = 1
            source = emit_cuda_device_ir(ir, altered, ok, message)
            call require(.not. ok, 'accepted non-strict rewrite policy')
        end do
        altered = spec
        deallocate(altered%arg_types)
        source = emit_cuda_device_ir(ir, altered, ok, message)
        call require(.not. ok, 'accepted absent argument types')
        altered = spec
        altered%output_types = [TYPE_UINT64]
        source = emit_cuda_device_ir(ir, altered, ok, message)
        call require(.not. ok, 'accepted wrong type array length')
        source = emit_fortran_kernel_ir(ir, spec, ok, message)
        call require(.not. ok, 'Fortran silently lost types')
        source = emit_python_kernel_ir(ir, spec, ok, message)
        call require(.not. ok, 'Python silently lost types')
        source = emit_cuda_batch_ir(ir, spec, ok, message)
        call require(.not. ok, 'batch silently lost types')
        call write_kernel_ir_file('typed-must-not-exist.fgir', ir, spec, ok, message)
        call require(.not. ok, 'v1 silently lost types')
        inquire(file='typed-must-not-exist.fgir', exist=exists)
        call require(.not. exists, 'v1 touched file before rejecting typed graph')
        open(newunit=unit, file='typed-preserve.fgir', status='replace')
        write(unit, '(a)') 'preserve this content'
        close(unit)
        call write_kernel_ir_file('typed-preserve.fgir', ir, spec, ok, message)
        call require(.not. ok, 'v1 accepted typed overwrite')
        open(newunit=unit, file='typed-preserve.fgir', status='old')
        read(unit, '(a)') preserved
        close(unit, status='delete')
        call require(trim(preserved) == 'preserve this content', &
            'v1 truncated existing file before refusing')
    end subroutine refusals

    subroutine reject(graph, label)
        type(kernel_ir_t), intent(in) :: graph
        character(*), intent(in) :: label
        source = emit_cuda_device_ir(graph, spec, ok, message)
        call require(.not. ok, 'accepted '//label)
    end subroutine reject

    subroutine require(condition, why)
        logical, intent(in) :: condition
        character(*), intent(in) :: why
        if (.not. condition) then
            print '(a)', 'FAIL: '//why
            error stop 1
        end if
    end subroutine require
end program test_typed_scalar
