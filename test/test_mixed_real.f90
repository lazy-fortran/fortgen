program test_mixed_real
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fortgen_kernel_ir
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_cuda_device_ir, &
        emit_fortran_kernel_ir, emit_metal_device_ir, emit_python_kernel_ir, &
        PRECISION_REAL32
    use fortgen_string, only: str, str_t, chars
    implicit none
    type(kernel_ir_t) :: ir, bad
    type(kernel_emit_spec_t) :: spec, altered
    type(str_t) :: source
    character(:), allocatable :: message
    character(4096) :: path
    integer :: count, used, x, y, x32, y32, bulk32, bulk64, product64, residual
    integer :: corrected, half, two, zero, positive_x, positive_y, valid, chosen
    integer :: literal, large, truth, lie, flag, roundtrip, mode, unit
    logical :: ok

    call kernel_ir_allocate(ir, 40, 80, 8)
    count = 0; used = 0
    x = node(IR_SYMBOL, TYPE_REAL64, name='x')
    y = node(IR_SYMBOL, TYPE_REAL64, name='y')
    x32 = node(IR_CAST, TYPE_REAL32, [x])
    y32 = node(IR_CAST, TYPE_REAL32, [y])
    bulk32 = node(IR_MUL, TYPE_REAL32, [x32, y32])
    bulk64 = node(IR_CAST, TYPE_REAL64, [bulk32])
    product64 = node(IR_MUL, TYPE_REAL64, [x, y])
    residual = node(IR_SUB, TYPE_REAL64, [product64, bulk64])
    corrected = node(IR_ADD, TYPE_REAL64, [bulk64, residual])
    two = node(IR_LITERAL, TYPE_REAL64, value=2.0_real64)
    half = node(IR_DIV, TYPE_REAL64, [corrected, two])
    zero = node(IR_LITERAL, TYPE_REAL64)
    positive_x = node(IR_GT, TYPE_BOOL, [x, zero])
    positive_y = node(IR_GE, TYPE_BOOL, [y, zero])
    valid = node(IR_AND, TYPE_BOOL, [positive_x, positive_y])
    truth = node(IR_LITERAL, TYPE_BOOL)
    ir%nodes(truth)%bool_value = .true.
    lie = node(IR_LITERAL, TYPE_BOOL)
    flag = node(IR_EQ, TYPE_BOOL, [valid, truth])
    flag = node(IR_NE, TYPE_BOOL, [flag, lie])
    flag = node(IR_NOT, TYPE_BOOL, [flag])
    flag = node(IR_NOT, TYPE_BOOL, [flag])
    flag = node(IR_OR, TYPE_BOOL, [flag, lie])
    chosen = node(IR_SELECT, TYPE_REAL64, [valid, corrected, zero])
    literal = node(IR_LITERAL, TYPE_REAL64, value=1099511627777.0_real64)
    large = node(IR_LITERAL, TYPE_REAL64, value=1.0e100_real64)
    roundtrip = node(IR_CAST, TYPE_REAL64, [x32])
    ir%nodes = ir%nodes(:count)
    ir%operands = ir%operands(:used)
    ir%n_nodes = count; ir%n_operands = used
    ir%outputs = [bulk32, residual, chosen, half, literal, large, flag, roundtrip]
    spec%name = str('mixed_correction')
    spec%args = [str('x'), str('y')]
    spec%arg_types = [TYPE_REAL64, TYPE_REAL64]
    spec%outputs = [str('bulk'), str('residual'), str('corrected'), &
        str('half'), str('literal'), str('large'), str('valid'), str('rounded_x')]
    spec%output_types = ir%nodes(ir%outputs)%scalar_type
    spec%precision = PRECISION_REAL32
    spec%policy%small_power_limit = 0
    spec%policy%fold_exact_constants = .false.
    spec%policy%eliminate_constant_divisions = .false.
    spec%policy%shape_fma = .false.
    spec%generator = str('test_mixed_real')
    spec%pure_procedure = .true.
    source = emit_cuda_device_ir(ir, spec, ok, message)
    call require(ok, message)
    source = emit_fortran_kernel_ir(ir, spec, ok, message)
    call require(ok, message)
    call refusals()
    call intrinsic_shadowing()
    do mode = 1, min(2, command_argument_count())
        call get_command_argument(mode, path)
        if (mode == 1) source = emit_cuda_device_ir(ir, spec, ok, message)
        if (mode == 2) source = emit_fortran_kernel_ir(ir, spec, ok, message)
        call require(ok, message)
        open(newunit=unit, file=trim(path), status='replace')
        write(unit, '(a)') chars(source)
        close(unit)
    end do
    print '(a)', 'mixed real validation passed'
contains
    integer function node(op, scalar, children, name, value) result(id)
        integer, intent(in) :: op, scalar
        integer, intent(in), optional :: children(:)
        character(*), intent(in), optional :: name
        real(real64), intent(in), optional :: value
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
    end function node

    subroutine refusals()
        source = emit_metal_device_ir(ir, spec, ok, message)
        call require(.not. ok .and. index(message, 'no native real64') > 0, &
            'Metal did not explicitly refuse native real64')
        call require(len(chars(source)) == 0, 'Metal returned partial source')
        source = emit_python_kernel_ir(ir, spec, ok, message)
        call require(.not. ok, 'Python silently accepted typed real64')
        bad = ir
        bad%nodes(product64)%scalar_type = TYPE_REAL32
        source = emit_cuda_device_ir(bad, spec, ok, message)
        call require(.not. ok, 'implicit mixed-width multiplication accepted')
        bad = ir
        bad%nodes(two)%value = ieee_value(0.0_real64, ieee_quiet_nan)
        source = emit_cuda_device_ir(bad, spec, ok, message)
        call require(.not. ok, 'nonfinite real64 literal accepted')
        bad = ir
        bad%nodes(product64)%scalar_type = TYPE_REAL64 + 1
        source = emit_cuda_device_ir(bad, spec, ok, message)
        call require(.not. ok, 'invalid scalar type accepted')
        altered = spec
        altered%arg_types(1) = TYPE_REAL32
        source = emit_cuda_device_ir(ir, altered, ok, message)
        call require(.not. ok, 'incorrect double argument type accepted')
        altered = spec
        altered%output_types(2) = TYPE_REAL32
        source = emit_fortran_kernel_ir(ir, altered, ok, message)
        call require(.not. ok, 'incorrect real64 output type accepted')
        altered = spec
        altered%policy%shape_fma = .true.
        source = emit_fortran_kernel_ir(ir, altered, ok, message)
        call require(.not. ok, 'Fortran accepted non-strict policy')
        ! Explicit types control precision even for an otherwise unused argument.
        altered = spec
        altered%args = [str('x'), str('y'), str('unused')]
        altered%arg_types = [TYPE_REAL64, TYPE_REAL64, TYPE_UINT32]
        source = emit_fortran_kernel_ir(ir, altered, ok, message)
        call require(.not. ok, 'unused unsigned Fortran argument accepted')
        source = emit_metal_device_ir(ir, altered, ok, message)
        call require(.not. ok, 'Metal accepted real64 arguments')
    end subroutine refusals

    subroutine intrinsic_shadowing()
        type(kernel_ir_t) :: shadow, temporary
        type(kernel_emit_spec_t) :: shadow_spec
        character(len=6), parameter :: names(6) = &
            [character(len=6) :: 'real', 'MERGE', 'real32', 'REAL64', 'ReAl', 'mErGe']
        integer :: j, k
        do j = 1, size(names)
            shadow = ir
            shadow_spec = spec
            shadow_spec%args(1) = str(trim(names(j)))
            shadow%nodes(x)%name = shadow_spec%args(1)
            source = emit_fortran_kernel_ir(shadow, shadow_spec, ok, message)
            call require(.not. ok .and. index(message, 'shadows') > 0, &
                'Fortran accepted shadowing argument '//trim(names(j)))
            call require(len(chars(source)) == 0, 'Fortran returned partial source')
            source = emit_cuda_device_ir(shadow, shadow_spec, ok, message)
            call require(ok, 'Fortran name refusal leaked into CUDA: '//message)
            shadow_spec = spec
            shadow_spec%outputs(1) = str(trim(names(j)))
            source = emit_fortran_kernel_ir(ir, shadow_spec, ok, message)
            call require(.not. ok .and. index(message, 'shadows') > 0, &
                'Fortran accepted shadowing output '//trim(names(j)))
            source = emit_cuda_device_ir(ir, shadow_spec, ok, message)
            call require(ok, 'CUDA rejected valid output: '//message)
            shadow_spec = spec
            shadow_spec%name = str(trim(names(j)))
            source = emit_fortran_kernel_ir(ir, shadow_spec, ok, message)
            call require(.not. ok .and. index(message, 'shadows') > 0, &
                'Fortran accepted shadowing procedure '//trim(names(j)))
        end do
        call kernel_ir_allocate(temporary, 32, 1, 1)
        do k = 1, 32
            temporary%nodes(k)%operation = IR_LITERAL
            temporary%nodes(k)%scalar_type = TYPE_REAL64
        end do
        temporary%nodes(32)%operation = IR_CAST
        temporary%nodes(32)%first_operand = 1
        temporary%nodes(32)%n_operands = 1
        temporary%operands = [1]
        temporary%outputs = [32]
        shadow_spec = spec
        shadow_spec%args = [str('unused')]
        shadow_spec%arg_types = [TYPE_REAL64]
        shadow_spec%outputs = [str('result')]
        shadow_spec%output_types = [TYPE_REAL64]
        shadow_spec%temp_prefix = str('real')
        source = emit_fortran_kernel_ir(temporary, shadow_spec, ok, message)
        call require(.not. ok .and. index(message, 'temporary shadows') > 0, &
            'Fortran accepted temporary named real32')
        source = emit_cuda_device_ir(temporary, shadow_spec, ok, message)
        call require(ok, 'CUDA rejected real32 temporary: '//message)
    end subroutine intrinsic_shadowing

    subroutine require(condition, why)
        logical, intent(in) :: condition
        character(*), intent(in) :: why
        if (.not. condition) then
            print '(a)', 'FAIL: '//why
            error stop 1
        end if
    end subroutine require
end program test_mixed_real
