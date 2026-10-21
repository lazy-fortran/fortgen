module fortgen_kernel_emit
    !! Emit scalar Fortran and CUDA leaves and Python functions from the
    !! shared FortGen kernel IR.
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortgen_kernel_ir, only: kernel_ir_t, kernel_ir_node_t, IR_LITERAL, &
        IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, IR_FUNCTION
    use fortgen_string, only: str_t, strbuf_t, str, chars
    use fortgen_functions, only: fortran_function_supported, &
        fortran_function_arity_ok, fortran_function_uses_special, &
        fortran_function_spelling
    use fortgen_kernel_target, only: TARGET_DEFAULT_VALUE => TARGET_DEFAULT, &
        TARGET_FORTRAN_CPU_VALUE => TARGET_FORTRAN_CPU, &
        TARGET_FORTRAN_OPENMP_TARGET_VALUE => TARGET_FORTRAN_OPENMP_TARGET, &
        TARGET_FORTRAN_OPENACC_VALUE => TARGET_FORTRAN_OPENACC, &
        TARGET_DUAL_VALUE => TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC, &
        TARGET_CUDA_VALUE => TARGET_CUDA, &
        target_directives, target_is_valid, target_name
    use fortgen_precision, only: PRECISION_REAL64, PRECISION_REAL32, &
        PRECISION_MIXED, precision_is_valid, precision_name
    use fortgen_names, only: valid_fortran_name, same_fortran_name, map_fortran_names
    use fortgen_buffer, only: buffer_t
    use fortgen_layout, only: wrap_fortran_source
    implicit none
    private

    public :: kernel_emission_policy_t, kernel_emit_spec_t
    public :: emit_fortran_kernel_ir, emit_cuda_device_ir, emit_python_kernel_ir
    public :: emit_cuda_batch_ir
    public :: TARGET_DEFAULT, TARGET_FORTRAN_CPU
    public :: TARGET_FORTRAN_OPENMP_TARGET, TARGET_FORTRAN_OPENACC
    public :: TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC, TARGET_CUDA
    public :: target_name
    public :: PRECISION_REAL64, PRECISION_REAL32, PRECISION_MIXED
    public :: precision_name

    integer, parameter :: TARGET_DEFAULT = TARGET_DEFAULT_VALUE
    integer, parameter :: TARGET_FORTRAN_CPU = TARGET_FORTRAN_CPU_VALUE
    integer, parameter :: TARGET_FORTRAN_OPENMP_TARGET = TARGET_FORTRAN_OPENMP_TARGET_VALUE
    integer, parameter :: TARGET_FORTRAN_OPENACC = TARGET_FORTRAN_OPENACC_VALUE
    integer, parameter :: TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC = TARGET_DUAL_VALUE
    integer, parameter :: TARGET_CUDA = TARGET_CUDA_VALUE
    integer, parameter :: dp = real64
    integer, parameter :: BACKEND_FORTRAN = 1
    integer, parameter :: BACKEND_CUDA = 2
    integer, parameter :: BACKEND_PYTHON = 3

    type :: kernel_emission_policy_t
        integer :: small_power_limit = 3
        logical :: fold_exact_constants = .true.
        logical :: eliminate_constant_divisions = .true.
        logical :: shape_fma = .true.
    end type kernel_emission_policy_t

    type :: kernel_emit_spec_t
        type(str_t) :: name
        type(str_t), allocatable :: args(:)
        type(str_t), allocatable :: outputs(:)
        type(str_t) :: temp_prefix
        integer :: target = TARGET_DEFAULT
        type(kernel_emission_policy_t) :: policy
        integer :: precision = PRECISION_REAL64
        logical :: pure_procedure = .false.
        type(str_t) :: special_module
        type(str_t) :: producer
        type(str_t) :: generator
        type(str_t) :: generator_revision
        type(str_t) :: regenerate_command
        !! Python backend only: optional docstring of the emitted function.
        type(str_t) :: docstring
    end type kernel_emit_spec_t

contains

    function emit_fortran_kernel_ir(ir, spec, ok, message) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(kernel_ir_t) :: mapped_ir, prepared
        type(kernel_emit_spec_t) :: mapped_spec
        type(str_t), allocatable :: original_names(:), emitted_names(:)
        logical, allocatable :: changed_names(:)

        call prepare_fortran_ir(ir, spec, mapped_ir, mapped_spec, original_names, &
            emitted_names, changed_names, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        call validate(mapped_ir, mapped_spec, BACKEND_FORTRAN, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        call apply_emission_policy(mapped_ir, mapped_spec%policy, prepared, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        source = emit_source(prepared, mapped_spec, BACKEND_FORTRAN, original_names, &
            emitted_names, changed_names)
    end function emit_fortran_kernel_ir

    function emit_cuda_device_ir(ir, spec, ok, message) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(kernel_ir_t) :: prepared

        call validate(ir, spec, BACKEND_CUDA, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        call apply_emission_policy(ir, spec%policy, prepared, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        source = emit_source(prepared, spec, BACKEND_CUDA)
    end function emit_cuda_device_ir

    function emit_cuda_batch_ir(ir, spec, ok, message) result(source)
        !! Packed structure-of-arrays boundary: field j starts at j*n.
        !! The caller owns allocation, stream, launch geometry and reductions.
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source, leaf
        type(strbuf_t) :: b
        character(:), allocatable :: input_type, output_type, name
        character(:), allocatable :: count_name, index_name, input_name, output_name
        integer :: j

        source = str('')
        leaf = emit_cuda_device_ir(ir, spec, ok, message)
        if (.not. ok) return
        name = chars(spec%name)
        ok = .false.
        message = 'CUDA batch name exceeds 63 characters with _batch suffix'
        if (len(name) > 57) return
        ! Only rename a local when it would hide the scalar device procedure.
        count_name = 'n'; index_name = 'i'
        input_name = 'inputs'; output_name = 'outputs'
        if (name == 'n') count_name = 'fg_batch_n'
        if (name == 'i') index_name = 'fg_batch_i'
        if (name == 'inputs') input_name = 'fg_batch_inputs'
        if (name == 'outputs') output_name = 'fg_batch_outputs'
        input_type = 'double'
        output_type = 'double'
        if (spec%precision /= PRECISION_REAL64) input_type = 'float'
        if (spec%precision == PRECISION_REAL32) output_type = 'float'
        call b%append('#include <cstddef>')
        call b%newline()
        call b%append('#include <cmath>')
        call b%newline()
        call b%append(chars(leaf))
        call b%newline()
        call b%append('static __global__ void '//name//'_batch(')
        call b%newline()
        call b%append('    std::size_t '//count_name//', const '//input_type// &
            '* '//input_name//', '//output_type//'* '//output_name//') {')
        call b%newline()
        call b%append('    const std::size_t '//index_name//' = std::size_t(blockIdx.x)'// &
            '*blockDim.x + threadIdx.x;')
        call b%newline()
        call b%append('    if ('//index_name//' >= '//count_name//') return;')
        call b%newline()
        call b%append('    '//name//'(')
        call b%newline()
        do j = 1, size(spec%args)
            call b%append('        '//input_name//'['//chars(str(j - 1))// &
                '*'//count_name//' + '//index_name//']')
            if (j < size(spec%args) .or. size(spec%outputs) > 0) &
                call b%append(',')
            call b%newline()
        end do
        do j = 1, size(spec%outputs)
            call b%append('        &'//output_name//'['//chars(str(j - 1))// &
                '*'//count_name//' + '//index_name//']')
            if (j < size(spec%outputs)) call b%append(',')
            call b%newline()
        end do
        call b%append('    );')
        call b%newline()
        call b%append('}')
        call b%newline()
        source = b%to_str()
        ok = .true.
        message = ''
    end function emit_cuda_batch_ir

    !> Emit a self-contained Python function `def name(args):` returning one
    !> value, or a tuple in declared output order. Arithmetic broadcasts over
    !> NumPy arrays; functions and pi/e use `numpy`, imported inside the
    !> function only when needed. The imaginary unit `i` becomes `1j`.
    function emit_python_kernel_ir(ir, spec, ok, message) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(kernel_ir_t) :: prepared

        call validate(ir, spec, BACKEND_PYTHON, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        call apply_emission_policy(ir, spec%policy, prepared, ok, message)
        if (.not. ok) then
            source = str("")
            return
        end if
        source = emit_python_source(prepared, spec)
    end function emit_python_kernel_ir

    function emit_python_source(ir, spec) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        type(str_t) :: source
        type(strbuf_t) :: b
        character(:), allocatable :: prefix, rhs, producer, message
        logical :: ok
        logical, allocatable :: skip_nodes(:)
        integer, allocatable :: use_counts(:)
        integer :: k

        call count_ir_uses(ir, use_counts)
        call mark_fma_children(ir, spec%policy, use_counts, skip_nodes)
        prefix = chars(spec%temp_prefix)
        if (len(prefix) == 0) prefix = "t"
        producer = chars(spec%producer)
        if (len(producer) == 0) producer = "fortgen"

        call b%append("# Generated by "//producer//". Do not edit.")
        call b%newline()
        call append_provenance(b, spec, "#")
        call b%append("def ")
        call b%append(chars(spec%name))
        call b%append("(")
        do k = 1, size(spec%args)
            if (k > 1) call b%append(", ")
            call b%append(chars(spec%args(k)))
        end do
        call b%append("):")
        call b%newline()
        if (len(chars(spec%docstring)) > 0) then
            call b%append('    """'//chars(spec%docstring)//'"""')
            call b%newline()
        end if
        if (python_needs_numpy(ir)) then
            call b%append("    import numpy")
            call b%newline()
        end if
        do k = 1, ir%n_nodes
            if (.not. is_compound(ir%nodes(k)%operation)) cycle
            if (skip_nodes(k)) cycle
            rhs = render_node(ir, k, prefix, BACKEND_PYTHON, spec%policy, &
                use_counts, PRECISION_REAL64, ok, message)
            if (.not. ok) error stop "validated kernel IR became unrenderable"
            call b%append("    "//prefix//chars(str(k))//" = "//rhs)
            call b%newline()
        end do
        call b%append("    return ")
        if (size(ir%outputs) > 1) call b%append("(")
        do k = 1, size(ir%outputs)
            if (k > 1) call b%append(", ")
            call b%append(operand_reference(ir, ir%outputs(k), prefix, &
                BACKEND_PYTHON, PRECISION_REAL64))
        end do
        if (size(ir%outputs) > 1) call b%append(")")
        call b%newline()
        source = b%to_str()
    end function emit_python_source

    logical function python_needs_numpy(ir) result(needed)
        type(kernel_ir_t), intent(in) :: ir
        integer :: k

        needed = .false.
        do k = 1, ir%n_nodes
            if (ir%nodes(k)%operation == IR_FUNCTION) needed = .true.
            if (ir%nodes(k)%operation == IR_CONSTANT) then
                if (chars(ir%nodes(k)%name) /= "i") needed = .true.
            end if
        end do
    end function python_needs_numpy

    !> Python keywords, soft keywords and the imported module name cannot be
    !> used as function, argument or temporary names.
    pure logical function python_reserved(name) result(reserved)
        character(*), intent(in) :: name

        select case (name)
        case ("False", "None", "True", "and", "as", "assert", "async", "await", &
                "break", "class", "continue", "def", "del", "elif", "else", &
                "except", "finally", "for", "from", "global", "if", "import", &
                "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", &
                "return", "try", "while", "with", "yield", "match", "case", &
                "type", "numpy")
            reserved = .true.
        case default
            reserved = .false.
        end select
    end function python_reserved

    subroutine prepare_fortran_ir(ir, spec, mapped_ir, mapped_spec, original_names, &
            emitted_names, changed_names, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        type(kernel_ir_t), intent(out) :: mapped_ir
        type(kernel_emit_spec_t), intent(out) :: mapped_spec
        type(str_t), allocatable, intent(out) :: original_names(:), emitted_names(:)
        logical, allocatable, intent(out) :: changed_names(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer :: k, j, n_args, n_outputs
        logical :: map_ok, found
        character(:), allocatable :: symbol_name

        ok = .false.
        message = ""
        mapped_ir = ir
        mapped_spec = spec
        if (.not. allocated(spec%args)) then
            message = "kernel emitter: argument names are not allocated"
            return
        end if
        if (.not. allocated(spec%outputs)) then
            message = "kernel emitter: output names are not allocated"
            return
        end if
        n_args = size(spec%args)
        n_outputs = size(spec%outputs)
        allocate (original_names(n_args + n_outputs))
        allocate (emitted_names(n_args + n_outputs))
        original_names(1:n_args) = spec%args
        if (n_outputs > 0) original_names(n_args + 1:) = spec%outputs
        do k = 1, size(original_names)
            original_names(k) = str(trim(chars(original_names(k))))
        end do
        call map_fortran_names(original_names, emitted_names, changed_names, &
            map_ok, message)
        if (.not. map_ok) return
        mapped_spec%args = emitted_names(1:n_args)
        if (n_outputs > 0) mapped_spec%outputs = emitted_names(n_args + 1:)

        do k = 1, ir%n_nodes
            if (ir%nodes(k)%operation /= IR_SYMBOL) cycle
            symbol_name = chars(ir%nodes(k)%name)
            found = .false.
            do j = 1, n_args
                if (symbol_name /= trim(chars(spec%args(j)))) cycle
                mapped_ir%nodes(k)%name = emitted_names(j)
                found = .true.
                exit
            end do
            if (.not. found) then
                do j = 1, n_args
                    if (.not. same_fortran_name(symbol_name, &
                        trim(chars(spec%args(j))))) cycle
                    mapped_ir%nodes(k)%name = emitted_names(j)
                    found = .true.
                    exit
                end do
            end if
            if (.not. found .and. .not. valid_fortran_name(symbol_name)) then
                message = "kernel emitter: symbol name is not a valid Fortran identifier: "// &
                    symbol_name
                return
            end if
        end do
        ok = .true.
    end subroutine prepare_fortran_ir

    !> Apply source-level policies to a validated, topological IR. The rewrite
    !> keeps one output node for every input node (or aliases it to a child),
    !> so provenance remains easy to inspect while neutral constants disappear
    !> before declarations and assignments are emitted.
    subroutine apply_emission_policy(input, policy, output, ok, message)
        type(kernel_ir_t), intent(in) :: input
        type(kernel_emission_policy_t), intent(in) :: policy
        type(kernel_ir_t), intent(out) :: output
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer, allocatable :: mapping(:), children(:)
        integer :: i, k, node_count, operand_count
        integer :: base, exponent, exponent_value
        integer :: empty_operands(0), pow_operands(2)
        real(dp) :: value
        logical :: folded

        call output%clear()
        ok = .false.
        message = ""
        if (policy%small_power_limit < 0) then
            message = "kernel emitter: small power limit is negative"
            return
        end if
        if (.not. allocated(input%nodes) .or. .not. allocated(input%operands) .or. &
            .not. allocated(input%outputs)) then
            message = "kernel emitter: IR storage is not allocated"
            return
        end if

        allocate (mapping(input%n_nodes), source=0)
        allocate (output%nodes(input%n_nodes))
        allocate (output%operands(input%n_operands))
        allocate (output%outputs(size(input%outputs)))
        node_count = 0
        operand_count = 0

        do i = 1, input%n_nodes
            select case (input%nodes(i)%operation)
            case (IR_LITERAL, IR_SYMBOL, IR_CONSTANT)
                call append_node(output, input%nodes(i)%operation, &
                    empty_operands, input%nodes(i)%value, &
                    input%nodes(i)%name, node_count, operand_count)
                mapping(i) = node_count
            case (IR_FUNCTION)
                allocate (children(input%nodes(i)%n_operands))
                do k = 1, size(children)
                    children(k) = mapping(input%operands( &
                        input%nodes(i)%first_operand + k - 1))
                end do
                call append_node(output, IR_FUNCTION, children, &
                    input%nodes(i)%value, input%nodes(i)%name, node_count, &
                    operand_count)
                mapping(i) = node_count
                deallocate (children)
            case (IR_ADD, IR_MUL)
                allocate (children(input%nodes(i)%n_operands))
                do k = 1, size(children)
                    children(k) = mapping(input%operands( &
                        input%nodes(i)%first_operand + k - 1))
                end do
                call fold_reduction(output, input%nodes(i)%operation, children, &
                    policy%fold_exact_constants, node_count, operand_count, &
                    mapping(i), folded)
                deallocate (children)
            case (IR_POW)
                base = mapping(input%operands(input%nodes(i)%first_operand))
                exponent = mapping(input%operands( &
                    input%nodes(i)%first_operand + 1))
                folded = .false.
                if (policy%fold_exact_constants) then
                    if (literal_equals(output, exponent, 1.0_dp)) then
                        mapping(i) = base
                        folded = .true.
                    else if (literal_equals(output, exponent, 0.0_dp) .and. &
                            .not. literal_equals(output, base, 0.0_dp)) then
                        call append_literal(output, 1.0_dp, node_count, &
                            operand_count, mapping(i))
                        folded = .true.
                    else if (literal_equals(output, base, 1.0_dp)) then
                        call append_literal(output, 1.0_dp, node_count, &
                            operand_count, mapping(i))
                        folded = .true.
                    else if (literal_equals(output, base, 0.0_dp) .and. &
                            literal_integer(output, exponent, exponent_value) .and. &
                            exponent_value > 0) then
                        call append_literal(output, 0.0_dp, node_count, &
                            operand_count, mapping(i))
                        folded = .true.
                    end if
                end if
                if (.not. folded .and. policy%eliminate_constant_divisions .and. &
                    literal_integer(output, exponent, exponent_value) .and. &
                    exponent_value == -1 .and. &
                    literal_value(output, base, value) .and. value /= 0.0_dp) then
                    call append_literal(output, 1.0_dp/value, node_count, &
                        operand_count, mapping(i))
                    folded = .true.
                end if
                if (.not. folded) then
                    pow_operands(1) = base
                    pow_operands(2) = exponent
                    call append_node(output, IR_POW, pow_operands, 0.0_dp, &
                        str(""), node_count, operand_count)
                    mapping(i) = node_count
                end if
            case default
                message = "kernel emitter: policy saw unknown IR operation"
                return
            end select
        end do

        do i = 1, size(input%outputs)
            output%outputs(i) = mapping(input%outputs(i))
        end do
        output%n_nodes = node_count
        output%n_operands = operand_count
        call trim_ir_storage(output)
        ok = .true.
    end subroutine apply_emission_policy

    subroutine append_node(ir, operation, operands, value, name, node_count, &
            operand_count)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(in) :: operation, operands(:)
        real(dp), intent(in) :: value
        type(str_t), intent(in) :: name
        integer, intent(inout) :: node_count, operand_count

        node_count = node_count + 1
        ir%nodes(node_count)%operation = operation
        ir%nodes(node_count)%value = value
        ir%nodes(node_count)%name = name
        ir%nodes(node_count)%first_operand = operand_count + 1
        ir%nodes(node_count)%n_operands = size(operands)
        if (size(operands) > 0) then
            ir%operands(operand_count + 1:operand_count + size(operands)) = operands
            operand_count = operand_count + size(operands)
        end if
    end subroutine append_node

    subroutine append_literal(ir, value, node_count, operand_count, index)
        type(kernel_ir_t), intent(inout) :: ir
        real(dp), intent(in) :: value
        integer, intent(inout) :: node_count, operand_count
        integer, intent(out) :: index
        integer :: empty_operands(0)

        call append_node(ir, IR_LITERAL, empty_operands, value, str(""), &
            node_count, operand_count)
        index = node_count
    end subroutine append_literal

    subroutine fold_reduction(ir, operation, children, enabled, node_count, &
            operand_count, index, folded)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(in) :: operation, children(:)
        logical, intent(in) :: enabled
        integer, intent(inout) :: node_count, operand_count
        integer, intent(out) :: index
        logical, intent(out) :: folded

        integer, allocatable :: retained(:)
        integer :: k, nretained
        real(dp) :: value

        folded = .false.
        nretained = 0
        allocate (retained(size(children)))
        if (enabled) then
            if (operation == IR_MUL) then
                do k = 1, size(children)
                    if (literal_equals(ir, children(k), 0.0_dp)) then
                        call append_literal(ir, 0.0_dp, node_count, &
                            operand_count, index)
                        deallocate (retained)
                        folded = .true.
                        return
                    end if
                end do
            end if
            do k = 1, size(children)
                if (operation == IR_ADD .and. literal_equals(ir, children(k), &
                    0.0_dp)) cycle
                if (operation == IR_MUL .and. literal_equals(ir, children(k), &
                    1.0_dp)) cycle
                nretained = nretained + 1
                retained(nretained) = children(k)
            end do
        else
            retained = children
            nretained = size(children)
        end if

        if (nretained == 0) then
            value = 0.0_dp
            if (operation == IR_MUL) value = 1.0_dp
            call append_literal(ir, value, node_count, operand_count, index)
            folded = .true.
        else if (nretained == 1) then
            index = retained(1)
            folded = .true.
        else if (nretained < size(children)) then
            call append_node(ir, operation, retained(1:nretained), 0.0_dp, &
                str(""), node_count, operand_count)
            index = node_count
            folded = .true.
        else
            call append_node(ir, operation, children, 0.0_dp, str(""), &
                node_count, operand_count)
            index = node_count
        end if
        deallocate (retained)
    end subroutine fold_reduction

    logical function literal_value(ir, index, value)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        real(dp), intent(out) :: value

        value = 0.0_dp
        literal_value = index >= 1 .and. index <= size(ir%nodes)
        if (.not. literal_value) then
            value = 0.0_dp
            return
        end if
        literal_value = ir%nodes(index)%operation == IR_LITERAL
        if (literal_value) value = ir%nodes(index)%value
    end function literal_value

    logical function literal_equals(ir, index, expected)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        real(dp), intent(in) :: expected
        real(dp) :: value

        literal_equals = literal_value(ir, index, value) .and. value == expected
    end function literal_equals

    logical function literal_integer(ir, index, value)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        integer, intent(out) :: value
        real(dp) :: real_value

        literal_integer = literal_value(ir, index, real_value)
        if (.not. literal_integer) then
            value = 0
            return
        end if
        if (.not. ieee_is_finite(real_value) .or. &
                abs(real_value) > real(huge(value), dp)) then
            literal_integer = .false.
            value = 0
            return
        end if
        value = nint(real_value)
        literal_integer = real(value, dp) == real_value
    end function literal_integer

    subroutine trim_ir_storage(ir)
        type(kernel_ir_t), intent(inout) :: ir
        type(kernel_ir_node_t), allocatable :: nodes(:)
        integer, allocatable :: operands(:)

        allocate (nodes(ir%n_nodes))
        if (ir%n_nodes > 0) nodes = ir%nodes(1:ir%n_nodes)
        allocate (operands(ir%n_operands))
        if (ir%n_operands > 0) operands = ir%operands(1:ir%n_operands)
        call move_alloc(nodes, ir%nodes)
        call move_alloc(operands, ir%operands)
    end subroutine trim_ir_storage

    function emit_source(ir, spec, backend, original_names, emitted_names, &
            changed_names) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        integer, intent(in) :: backend
        type(str_t), intent(in), optional :: original_names(:), emitted_names(:)
        logical, intent(in), optional :: changed_names(:)
        type(str_t) :: source
        type(strbuf_t) :: b
        character(:), allocatable :: prefix, rhs, producer
        logical :: ok, emit_openmp, emit_openacc
        logical, allocatable :: skip_nodes(:)
        integer, allocatable :: use_counts(:)
        character(:), allocatable :: message
        integer :: k

        call target_directives(spec%target, .false., .false., emit_openmp, &
            emit_openacc)
        call count_ir_uses(ir, use_counts)
        call mark_fma_children(ir, spec%policy, use_counts, skip_nodes)

        prefix = chars(spec%temp_prefix)
        if (len(prefix) == 0) prefix = "t"
        producer = chars(spec%producer)
        if (len(producer) == 0) producer = "fortgen"

        if (backend == BACKEND_FORTRAN) then
            call b%append("! Generated by "//producer//". Do not edit.")
            call b%newline()
            call append_provenance(b, spec, "!")
            call append_name_mapping(b, original_names, emitted_names, changed_names, "!")
            if (spec%pure_procedure) call b%append("pure ")
            call b%append("subroutine ")
            call b%append(chars(spec%name))
            call b%append("(")
            call append_arguments(b, spec, .false.)
            call b%append(")")
            call b%newline()
            if (emit_openmp) then
                call b%append("    !$omp declare target")
                call b%newline()
            end if
            if (emit_openacc) then
                call b%append("    !$acc routine seq")
                call b%newline()
            end if
            if (spec%precision == PRECISION_REAL32) then
                call b%append("    use, intrinsic :: iso_fortran_env, only: real32")
            else if (spec%precision == PRECISION_MIXED) then
                call b%append("    use, intrinsic :: iso_fortran_env, only: real32, real64")
            else
                call b%append("    use, intrinsic :: iso_fortran_env, only: real64")
            end if
            call b%newline()
            call append_special_use(b, ir, spec)
            call b%append("    implicit none")
            call b%newline()
            if (size(spec%args) > 0) then
                if (spec%precision == PRECISION_REAL32 .or. &
                    spec%precision == PRECISION_MIXED) then
                    call append_declaration(b, "real(real32), intent(in)", spec%args)
                else
                    call append_declaration(b, "real(real64), intent(in)", spec%args)
                end if
            end if
            if (size(spec%outputs) > 0) then
                if (spec%precision == PRECISION_MIXED) then
                    call append_declaration(b, "real(real64), intent(out)", spec%outputs)
                else if (spec%precision == PRECISION_REAL32) then
                    call append_declaration(b, "real(real32), intent(out)", spec%outputs)
                else
                    call append_declaration(b, "real(real64), intent(out)", spec%outputs)
                end if
            end if
            if (count_compounds(ir, skip_nodes) > 0) then
                call append_temporary_declaration(b, prefix, ir, skip_nodes, &
                    spec%precision)
            end if
        else
            call b%append("/* Generated by "//producer//". Do not edit. */")
            call b%newline()
            call append_provenance(b, spec, "//")
            call b%append("#include <math.h>")
            call b%newline()
            call b%append('extern "C" __device__ __forceinline__')
            call b%newline()
            call b%append("void ")
            call b%append(chars(spec%name))
            call b%append("(")
            call append_arguments(b, spec, .true.)
            call b%append(")")
            call b%newline()
            call b%append("{")
            call b%newline()
        end if

        call b%newline()
        do k = 1, ir%n_nodes
            if (.not. is_compound(ir%nodes(k)%operation)) cycle
            if (skip_nodes(k)) cycle
            rhs = render_node(ir, k, prefix, backend, spec%policy, use_counts, &
                spec%precision, ok, message)
            if (.not. ok) error stop "validated kernel IR became unrenderable"
            if (backend == BACKEND_FORTRAN) then
                call b%append("    ")
                call b%append(prefix)
                call b%append(chars(str(k)))
                call b%append(" = ")
                call b%append(rhs)
            else
                if (spec%precision == PRECISION_REAL32 .or. &
                    spec%precision == PRECISION_MIXED) then
                    call b%append("    const float ")
                else
                    call b%append("    const double ")
                end if
                call b%append(prefix)
                call b%append(chars(str(k)))
                call b%append(" = ")
                call b%append(rhs)
                call b%append(";")
            end if
            call b%newline()
        end do

        do k = 1, size(ir%outputs)
            if (backend == BACKEND_FORTRAN) then
                call b%append("    ")
                call b%append(chars(spec%outputs(k)))
                call b%append(" = ")
                call b%append(operand_reference(ir, ir%outputs(k), prefix, &
                    backend, spec%precision))
            else
                call b%append("    *")
                call b%append(chars(spec%outputs(k)))
                call b%append(" = ")
                call b%append(operand_reference(ir, ir%outputs(k), prefix, &
                    backend, spec%precision))
                call b%append(";")
            end if
            call b%newline()
        end do

        if (backend == BACKEND_FORTRAN) then
            call b%append("end subroutine ")
            call b%append(chars(spec%name))
        else
            call b%append("}")
        end if
        call b%newline()
        source = b%to_str()
        if (backend == BACKEND_FORTRAN) source = wrap_fortran_lines(source)
    end function emit_source

    function wrap_fortran_lines(source) result(wrapped)
        type(str_t), intent(in) :: source
        type(str_t) :: wrapped

        wrapped = str(wrap_fortran_source(chars(source)))
    end function wrap_fortran_lines

    subroutine append_special_use(b, ir, spec)
        type(strbuf_t), intent(inout) :: b
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        ! Special heads in the fixed order of the emitted only-list.
        character(len=10), parameter :: heads(4) = [character(len=10) :: &
            "besseli", "besselk", "elliptic_k", "elliptic_e"]
        logical :: used(size(heads)), first
        integer :: k, j
        character(:), allocatable :: module_name

        used = .false.
        do k = 1, ir%n_nodes
            if (ir%nodes(k)%operation /= IR_FUNCTION) cycle
            do j = 1, size(heads)
                if (chars(ir%nodes(k)%name) == trim(heads(j))) used(j) = .true.
            end do
        end do
        if (.not. any(used)) return

        module_name = chars(spec%special_module)
        if (len_trim(module_name) == 0) module_name = "fortnum_special"
        call b%append("    use "//trim(module_name)//", only: ")
        first = .true.
        do j = 1, size(heads)
            if (.not. used(j)) cycle
            if (.not. first) call b%append(", ")
            call b%append(fortran_function_spelling(trim(heads(j))))
            first = .false.
        end do
        call b%newline()
    end subroutine append_special_use

    subroutine append_name_mapping(b, original_names, emitted_names, changed_names, comment)
        type(strbuf_t), intent(inout) :: b
        type(str_t), intent(in), optional :: original_names(:), emitted_names(:)
        logical, intent(in), optional :: changed_names(:)
        character(*), intent(in) :: comment
        integer :: k

        if (.not. present(original_names) .or. .not. present(emitted_names) .or. &
            .not. present(changed_names)) return
        do k = 1, size(original_names)
            if (.not. changed_names(k)) cycle
            call b%append(comment//" Fortran symbol name mapping: ")
            call b%append(chars(original_names(k)))
            call b%append(" -> ")
            call b%append(chars(emitted_names(k)))
            call b%newline()
        end do
    end subroutine append_name_mapping

    subroutine validate(ir, spec, backend, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        integer, intent(in) :: backend
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: k, j, operand, first, last, order_value
        real(real32) :: single_value
        character(:), allocatable :: prefix, temporary, module_name

        ok = .false.
        message = ""
        if (.not. precision_is_valid(spec%precision)) then
            message = "kernel emitter: precision choice is invalid"
            return
        end if
        if (.not. valid_fortran_name(chars(spec%name))) then
            message = "kernel emitter: kernel name is invalid"
            return
        end if
        if (.not. target_is_valid(spec%target)) then
            message = "kernel emitter: target identity is invalid"
            return
        end if
        if (backend == BACKEND_FORTRAN .and. spec%target == TARGET_CUDA) then
            message = "kernel emitter: CUDA target requires CUDA emitter"
            return
        end if
        if (backend == BACKEND_CUDA .and. spec%target /= TARGET_DEFAULT .and. &
            spec%target /= TARGET_CUDA) then
            message = "kernel emitter: non-CUDA target requires Fortran emitter"
            return
        end if
        if (backend == BACKEND_PYTHON) then
            call validate_python_spec(spec, ok, message)
            if (.not. ok) return
            ok = .false.
        end if
        if (.not. allocated(spec%args)) then
            message = "kernel emitter: argument names are not allocated"
            return
        end if
        if (.not. allocated(spec%outputs)) then
            message = "kernel emitter: output names are not allocated"
            return
        end if
        if (size(spec%outputs) /= size(ir%outputs)) then
            message = "kernel emitter: output names do not match roots"
            return
        end if
        if (.not. allocated(ir%nodes) .or. .not. allocated(ir%operands)) then
            message = "kernel emitter: IR storage is not allocated"
            return
        end if
        if (ir%n_nodes /= size(ir%nodes) .or. &
            ir%n_operands /= size(ir%operands)) then
            message = "kernel emitter: IR counts do not match storage"
            return
        end if
        do k = 1, size(spec%args)
            if (.not. valid_fortran_name(chars(spec%args(k)))) then
                message = "kernel emitter: argument name is invalid"
                return
            end if
            if (duplicate_name(spec%args, k)) then
                message = "kernel emitter: argument names are duplicated"
                return
            end if
        end do
        do k = 1, size(spec%outputs)
            if (.not. valid_fortran_name(chars(spec%outputs(k)))) then
                message = "kernel emitter: output name is invalid"
                return
            end if
            if (duplicate_name(spec%outputs, k)) then
                message = "kernel emitter: output names are duplicated"
                return
            end if
            if (is_argument(chars(spec%outputs(k)), spec%args)) then
                message = "kernel emitter: input and output names overlap"
                return
            end if
        end do
        prefix = chars(spec%temp_prefix)
        if (len(prefix) == 0) prefix = "t"
        if (.not. valid_fortran_name(prefix)) then
            message = "kernel emitter: temporary prefix is invalid"
            return
        end if
        if (backend == BACKEND_FORTRAN .and. has_special_function(ir)) then
            module_name = chars(spec%special_module)
            if (len_trim(module_name) > 0) then
                if (.not. valid_fortran_name(trim(module_name))) then
                    message = "kernel emitter: special module name is invalid"
                    return
                end if
            end if
        end if

        do k = 1, ir%n_nodes
            if (backend == BACKEND_PYTHON .and. &
                ir%nodes(k)%operation == IR_SYMBOL) then
                if (python_reserved(chars(ir%nodes(k)%name))) then
                    message = "kernel emitter: symbol name is reserved in Python: "// &
                        chars(ir%nodes(k)%name)
                    return
                end if
            end if
            first = ir%nodes(k)%first_operand
            last = first + ir%nodes(k)%n_operands - 1
            if (ir%nodes(k)%n_operands > 0) then
                if (first < 1 .or. last > ir%n_operands) then
                    message = "kernel emitter: operand slice is invalid"
                    return
                end if
                do j = first, last
                    operand = ir%operands(j)
                    if (operand < 1 .or. operand >= k) then
                        message = "kernel emitter: IR is not topological"
                        return
                    end if
                end do
            end if
            select case (ir%nodes(k)%operation)
            case (IR_LITERAL, IR_SYMBOL, IR_CONSTANT)
                if (ir%nodes(k)%n_operands /= 0) then
                    message = "kernel emitter: atom has operands"
                    return
                end if
            case (IR_ADD, IR_MUL)
                if (ir%nodes(k)%n_operands < 1) then
                    message = "kernel emitter: reduction has no operands"
                    return
                end if
            case (IR_POW)
                if (ir%nodes(k)%n_operands /= 2) then
                    message = "kernel emitter: power does not have two operands"
                    return
                end if
            case (IR_FUNCTION)
                if (ir%nodes(k)%n_operands < 1) then
                    message = "kernel emitter: function has no operands"
                    return
                end if
                if (.not. function_arity_ok(chars(ir%nodes(k)%name), &
                    ir%nodes(k)%n_operands)) then
                    message = "kernel emitter: wrong arity for function "// &
                        chars(ir%nodes(k)%name)
                    return
                end if
                if (backend == BACKEND_FORTRAN .and. &
                    integer_order_function(chars(ir%nodes(k)%name))) then
                    if (.not. literal_integer(ir, ir%operands(first), order_value)) then
                        message = "kernel emitter: Bessel order must be an integer literal"
                        return
                    end if
                end if
                if (.not. supported_function(chars(ir%nodes(k)%name), backend)) then
                    message = "kernel emitter: unsupported "//backend_name(backend)// &
                        " function "//chars(ir%nodes(k)%name)
                    return
                end if
            case default
                message = "kernel emitter: unknown IR operation"
                return
            end select
            if (ir%nodes(k)%operation == IR_SYMBOL) then
                if (.not. valid_fortran_name(chars(ir%nodes(k)%name))) then
                    message = "kernel emitter: symbol name is not a valid Fortran/CUDA identifier: "// &
                        chars(ir%nodes(k)%name)
                    return
                end if
                if (.not. is_argument(chars(ir%nodes(k)%name), spec%args)) then
                    message = "kernel emitter: symbol is not an input argument: "// &
                        chars(ir%nodes(k)%name)
                    return
                end if
            else if (ir%nodes(k)%operation == IR_CONSTANT) then
                if (.not. supported_constant(chars(ir%nodes(k)%name), backend)) then
                    message = "kernel emitter: unsupported "//backend_name(backend)// &
                        " constant "//chars(ir%nodes(k)%name)
                    return
                end if
            end if
            if ((spec%precision == PRECISION_REAL32 .or. &
                spec%precision == PRECISION_MIXED) .and. &
                ir%nodes(k)%operation == IR_LITERAL) then
                single_value = real(ir%nodes(k)%value, real32)
                if (.not. ieee_is_finite(single_value)) then
                    message = "kernel emitter: literal is not finite in real32"
                    return
                end if
            end if
            if (is_compound(ir%nodes(k)%operation)) then
                temporary = prefix//chars(str(k))
                if (backend == BACKEND_FORTRAN) then
                    if (.not. valid_fortran_name(temporary)) then
                        message = "kernel emitter: temporary name exceeds Fortran's "// &
                            "63-character limit: "//temporary
                        return
                    end if
                end if
                if (is_argument(temporary, spec%args) .or. &
                    is_argument(temporary, spec%outputs)) then
                    message = "kernel emitter: temporary collides with an argument: "// &
                        temporary
                    return
                end if
            end if
        end do
        do k = 1, size(ir%outputs)
            if (ir%outputs(k) < 1 .or. ir%outputs(k) > ir%n_nodes) then
                message = "kernel emitter: output root is invalid"
                return
            end if
        end do
        ok = .true.
    end subroutine validate

    !> Python-only specification checks: binary64 arithmetic, no device
    !> target, no reserved names and a docstring that cannot end its literal.
    subroutine validate_python_spec(spec, ok, message)
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character(:), allocatable :: docstring
        integer :: k

        ok = .false.
        message = ""
        if (spec%target /= TARGET_DEFAULT) then
            message = "kernel emitter: Python emitter takes no device target"
            return
        end if
        if (spec%precision /= PRECISION_REAL64) then
            message = "kernel emitter: Python emitter supports only real64"
            return
        end if
        if (python_reserved(chars(spec%name))) then
            message = "kernel emitter: kernel name is reserved in Python"
            return
        end if
        do k = 1, size(spec%args)
            if (python_reserved(chars(spec%args(k)))) then
                message = "kernel emitter: argument name is reserved in Python: "// &
                    chars(spec%args(k))
                return
            end if
        end do
        docstring = chars(spec%docstring)
        if (index(docstring, '"') > 0 .or. index(docstring, "\") > 0) then
            message = "kernel emitter: Python docstring contains a quote or backslash"
            return
        end if
        do k = 1, len(docstring)
            if (iachar(docstring(k:k)) < 32 .and. docstring(k:k) /= achar(10)) then
                message = "kernel emitter: Python docstring contains a control "// &
                    "character"
                return
            end if
        end do
        ok = .true.
    end subroutine validate_python_spec

    logical function duplicate_name(names, index)
        type(str_t), intent(in) :: names(:)
        integer, intent(in) :: index
        integer :: k

        duplicate_name = .false.
        do k = 1, index - 1
            if (chars(names(k)) == chars(names(index))) then
                duplicate_name = .true.
                return
            end if
        end do
    end function duplicate_name

    recursive function render_node(ir, index, prefix, backend, policy, use_counts, &
            precision, ok, message) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index, backend
        character(*), intent(in) :: prefix
        type(kernel_emission_policy_t), intent(in) :: policy
        integer, intent(in) :: use_counts(:)
        integer, intent(in) :: precision
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character(:), allocatable :: text
        type(strbuf_t) :: b
        character(:), allocatable :: name, rendered_child
        integer :: k, operand, exponent
        logical :: integer_power

        ok = .false.
        message = ""
        select case (ir%nodes(index)%operation)
        case (IR_LITERAL)
            text = literal_text(ir%nodes(index)%value, backend, precision)
        case (IR_SYMBOL)
            text = chars(ir%nodes(index)%name)
        case (IR_CONSTANT)
            name = chars(ir%nodes(index)%name)
            text = constant_text(name, backend, precision)
        case (IR_ADD, IR_MUL)
            call b%append("(")
            do k = 1, ir%nodes(index)%n_operands
                if (k > 1) then
                    if (ir%nodes(index)%operation == IR_ADD) then
                        call b%append(" + ")
                    else
                        call b%append(" * ")
                    end if
                end if
                operand = ir%operands(ir%nodes(index)%first_operand + k - 1)
                if (should_inline_fma(ir, index, operand, policy, use_counts)) then
                    rendered_child = render_node(ir, operand, prefix, backend, &
                        policy, use_counts, precision, ok, message)
                    if (.not. ok) return
                    call b%append(rendered_child)
                else
                    call b%append(operand_reference(ir, operand, prefix, backend, &
                        precision))
                end if
            end do
            call b%append(")")
            text = chars(b%to_str())
        case (IR_POW)
            operand = ir%operands(ir%nodes(index)%first_operand)
            integer_power = literal_integer(ir, &
                ir%operands(ir%nodes(index)%first_operand + 1), exponent)
            ! Preserve the integer power's real domain and IEEE signs without
            ! a general libm/libdevice power. Form the reciprocal first: the
            ! denominator square could overflow even when x**(-2) is nonzero.
            if (policy%small_power_limit > 0 .and. integer_power .and. exponent < 0 .and. &
                exponent >= -policy%small_power_limit) then
                call b%append("(")
                do k = 1, -exponent
                    if (k > 1) call b%append(" * ")
                    call b%append("("//literal_text(1.0_dp, backend, precision)//" / ")
                    call b%append(operand_reference(ir, operand, prefix, backend, precision))
                    call b%append(")")
                end do
                call b%append(")")
                text = chars(b%to_str())
                ok = .true.
                return
            end if
            if (policy%small_power_limit > 0 .and. integer_power .and. exponent >= 2 .and. &
                exponent <= policy%small_power_limit) then
                call b%append("(")
                do k = 1, exponent
                    if (k > 1) call b%append(" * ")
                    call b%append(operand_reference(ir, operand, prefix, backend, &
                        precision))
                end do
                call b%append(")")
                text = chars(b%to_str())
                ok = .true.
                return
            end if
            call b%append("(")
            call b%append(operand_reference(ir, operand, prefix, backend, precision))
            if (backend == BACKEND_PYTHON .and. integer_power) then
                call b%append(" ** "//chars(str(exponent))//")")
                text = chars(b%to_str())
                ok = .true.
                return
            end if
            if (backend == BACKEND_FORTRAN .or. backend == BACKEND_PYTHON) then
                call b%append(" ** ")
            else
                call b%append(", ")
                call b%append(operand_reference(ir, &
                    ir%operands(ir%nodes(index)%first_operand + 1), prefix, backend, &
                    precision))
                call b%append(")")
                text = "pow"//chars(b%to_str())
                ok = .true.
                return
            end if
            call b%append(operand_reference(ir, &
                ir%operands(ir%nodes(index)%first_operand + 1), prefix, backend, &
                precision))
            call b%append(")")
            text = chars(b%to_str())
        case (IR_FUNCTION)
            name = function_name(chars(ir%nodes(index)%name), backend, precision)
            if (backend == BACKEND_PYTHON .and. ir%nodes(index)%n_operands > 2) then
                ! numpy.maximum/minimum are binary: nest them left to right.
                operand = ir%operands(ir%nodes(index)%first_operand)
                text = operand_reference(ir, operand, prefix, backend, precision)
                do k = 2, ir%nodes(index)%n_operands
                    operand = ir%operands(ir%nodes(index)%first_operand + k - 1)
                    text = name//"("//text//", "// &
                        operand_reference(ir, operand, prefix, backend, precision)//")"
                end do
                ok = .true.
                return
            end if
            call b%append(name)
            call b%append("(")
            do k = 1, ir%nodes(index)%n_operands
                if (k > 1) call b%append(", ")
                operand = ir%operands(ir%nodes(index)%first_operand + k - 1)
                if (backend == BACKEND_FORTRAN .and. k == 1 .and. &
                    integer_order_function(chars(ir%nodes(index)%name))) then
                    call b%append(integer_order_text(ir, operand))
                else
                    call b%append(operand_reference(ir, operand, prefix, backend, &
                        precision))
                end if
            end do
            call b%append(")")
            text = chars(b%to_str())
        case default
            text = ""
            message = "kernel emitter: cannot render unknown operation"
            return
        end select
        ok = .true.
    end function render_node

    subroutine count_ir_uses(ir, use_counts)
        type(kernel_ir_t), intent(in) :: ir
        integer, allocatable, intent(out) :: use_counts(:)
        integer :: i, k, operand

        allocate (use_counts(ir%n_nodes), source=0)
        do i = 1, ir%n_nodes
            do k = 1, ir%nodes(i)%n_operands
                operand = ir%operands(ir%nodes(i)%first_operand + k - 1)
                use_counts(operand) = use_counts(operand) + 1
            end do
        end do
        do i = 1, size(ir%outputs)
            use_counts(ir%outputs(i)) = use_counts(ir%outputs(i)) + 1
        end do
    end subroutine count_ir_uses

    subroutine mark_fma_children(ir, policy, use_counts, skip_nodes)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emission_policy_t), intent(in) :: policy
        integer, intent(in) :: use_counts(:)
        logical, allocatable, intent(out) :: skip_nodes(:)
        integer :: i, k, operand

        allocate (skip_nodes(ir%n_nodes), source=.false.)
        if (.not. policy%shape_fma) return
        do i = 1, ir%n_nodes
            if (ir%nodes(i)%operation /= IR_ADD) cycle
            do k = 1, ir%nodes(i)%n_operands
                operand = ir%operands(ir%nodes(i)%first_operand + k - 1)
                if (should_inline_fma(ir, i, operand, policy, use_counts)) then
                    skip_nodes(operand) = .true.
                end if
            end do
        end do
    end subroutine mark_fma_children

    logical function should_inline_fma(ir, add_index, operand, policy, use_counts)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: add_index, operand
        type(kernel_emission_policy_t), intent(in) :: policy
        integer, intent(in) :: use_counts(:)

        should_inline_fma = .false.
        if (.not. policy%shape_fma) return
        if (ir%nodes(add_index)%operation /= IR_ADD) return
        if (operand < 1 .or. operand > ir%n_nodes) return
        should_inline_fma = ir%nodes(operand)%operation == IR_MUL .and. &
            ir%nodes(operand)%n_operands >= 2 .and. use_counts(operand) == 1
    end function should_inline_fma

    function operand_reference(ir, index, prefix, backend, precision) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index, backend
        character(*), intent(in) :: prefix
        integer, intent(in) :: precision
        character(:), allocatable :: text

        if (is_compound(ir%nodes(index)%operation)) then
            text = prefix//chars(str(index))
        else
            text = render_atom(ir%nodes(index), backend, precision)
        end if
    end function operand_reference

    function render_atom(node, backend, precision) result(text)
        type(kernel_ir_node_t), intent(in) :: node
        integer, intent(in) :: backend
        integer, intent(in) :: precision
        character(:), allocatable :: text

        select case (node%operation)
        case (IR_LITERAL)
            text = literal_text(node%value, backend, precision)
        case (IR_SYMBOL)
            text = chars(node%name)
        case (IR_CONSTANT)
            text = constant_text(chars(node%name), backend, precision)
        case default
            text = ""
        end select
    end function render_atom

    function literal_text(value, backend, precision) result(text)
        real(dp), intent(in) :: value
        integer, intent(in) :: backend
        integer, intent(in) :: precision
        character(:), allocatable :: text

        text = chars(str(value, "(es25.16e3)"))
        if (backend == BACKEND_FORTRAN) then
            if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
                text = text//"_real32"
            else
                text = text//"_real64"
            end if
        else if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
            text = text//"f"
        end if
        if (value < 0.0_dp) text = "("//text//")"
    end function literal_text

    function constant_text(name, backend, precision) result(text)
        character(*), intent(in) :: name
        integer, intent(in) :: backend
        integer, intent(in) :: precision
        character(:), allocatable :: text

        if (backend == BACKEND_PYTHON) then
            select case (name)
            case ("pi"); text = "numpy.pi"
            case ("e");  text = "numpy.e"
            case default; text = "1j"
            end select
        else if (name == "pi") then
            text = constant_pi(backend, precision)
        else
            text = constant_e(backend, precision)
        end if
    end function constant_text

    function constant_pi(backend, precision) result(text)
        integer, intent(in) :: backend
        integer, intent(in) :: precision
        character(:), allocatable :: text

        if (backend == BACKEND_FORTRAN) then
            if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
                text = "acos(-1.0_real32)"
            else
                text = "acos(-1.0_real64)"
            end if
        else if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
            text = "acosf(-1.0f)"
        else
            text = "acos(-1.0)"
        end if
    end function constant_pi

    function constant_e(backend, precision) result(text)
        integer, intent(in) :: backend
        integer, intent(in) :: precision
        character(:), allocatable :: text

        if (backend == BACKEND_FORTRAN) then
            if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
                text = "exp(1.0_real32)"
            else
                text = "exp(1.0_real64)"
            end if
        else if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
            text = "expf(1.0f)"
        else
            text = "exp(1.0)"
        end if
    end function constant_e

    function function_name(name, backend, precision) result(text)
        character(*), intent(in) :: name
        integer, intent(in) :: backend
        integer, intent(in) :: precision
        character(:), allocatable :: text

        if (backend == BACKEND_FORTRAN) then
            text = fortran_function_spelling(name)
        else if (backend == BACKEND_PYTHON) then
            text = "numpy."//python_function_spelling(name)
            return
        else
            text = name
        end if
        if (backend == BACKEND_CUDA) then
            if (name == "abs") text = "fabs"
            if (name == "gamma") text = "tgamma"
            if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
                select case (name)
                case ("sin", "cos", "tan", "asin", "acos", "atan", "sinh", &
                        "cosh", "tanh", "asinh", "acosh", "atanh", "exp", &
                        "log", "sqrt", "erf", "erfc")
                    text = text//"f"
                case ("atan2")
                    text = "atan2f"
                case ("abs")
                    text = "fabsf"
                case ("gamma")
                    text = "tgammaf"
                end select
            end if
        end if
    end function function_name

    !> NumPy spelling of the canonical functions the Python backend emits. An
    !> empty result means the function has no NumPy ufunc and is refused.
    pure function python_function_spelling(name) result(text)
        character(*), intent(in) :: name
        character(:), allocatable :: text

        select case (name)
        case ("sin", "cos", "tan", "sinh", "cosh", "tanh", "exp", "log", &
                "log10", "sqrt", "abs")
            text = name
        case ("asin", "acos", "atan", "atan2", "asinh", "acosh", "atanh")
            text = "arc"//name(2:)
        case ("max", "Max"); text = "maximum"
        case ("min", "Min"); text = "minimum"
        case default; text = ""
        end select
    end function python_function_spelling

    logical function supported_function(name, backend)
        character(*), intent(in) :: name
        integer, intent(in) :: backend

        supported_function = .false.
        if (backend == BACKEND_FORTRAN) then
            supported_function = fortran_function_supported(name)
            return
        end if
        if (backend == BACKEND_PYTHON) then
            supported_function = len(python_function_spelling(name)) > 0
            return
        end if
        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
                "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "exp", &
                "log", "sqrt", "abs", "erf", "erfc")
            supported_function = .true.
        case ("gamma")
            supported_function = backend == BACKEND_FORTRAN .or. &
                backend == BACKEND_CUDA
        end select
    end function supported_function

    logical function function_arity_ok(name, n)
        character(*), intent(in) :: name
        integer, intent(in) :: n

        function_arity_ok = fortran_function_arity_ok(name, n)
    end function function_arity_ok

    pure logical function integer_order_function(name) result(ok)
        character(*), intent(in) :: name

        ok = name == "besselj" .or. name == "bessely" .or. &
            name == "besseli" .or. name == "besselk"
    end function integer_order_function

    logical function has_special_function(ir) result(found)
        type(kernel_ir_t), intent(in) :: ir
        integer :: k

        found = .false.
        do k = 1, ir%n_nodes
            if (ir%nodes(k)%operation /= IR_FUNCTION) cycle
            if (fortran_function_uses_special(chars(ir%nodes(k)%name))) then
                found = .true.
                return
            end if
        end do
    end function has_special_function

    function integer_order_text(ir, index) result(text)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: index
        character(:), allocatable :: text
        character(64) :: buffer
        integer :: value

        value = nint(ir%nodes(index)%value)
        write (buffer, "(i0)") value
        text = trim(buffer)
    end function integer_order_text

    logical function supported_constant(name, backend)
        character(*), intent(in) :: name
        integer, intent(in) :: backend

        supported_constant = name == "pi" .or. name == "e"
        if (backend == BACKEND_PYTHON) supported_constant = supported_constant .or. &
            name == "i"
    end function supported_constant

    logical function is_argument(name, args)
        character(*), intent(in) :: name
        type(str_t), intent(in) :: args(:)
        integer :: k

        is_argument = .false.
        do k = 1, size(args)
            if (chars(args(k)) == name) then
                is_argument = .true.
                return
            end if
        end do
    end function is_argument

    logical function is_compound(operation)
        integer, intent(in) :: operation

        is_compound = operation == IR_ADD .or. operation == IR_MUL .or. &
            operation == IR_POW .or. operation == IR_FUNCTION
    end function is_compound

    integer function count_compounds(ir, skip_nodes)
        type(kernel_ir_t), intent(in) :: ir
        logical, intent(in), optional :: skip_nodes(:)
        integer :: k

        count_compounds = 0
        do k = 1, ir%n_nodes
            if (is_compound(ir%nodes(k)%operation)) count_compounds = &
                count_compounds + 1
            if (present(skip_nodes)) then
                if (skip_nodes(k) .and. is_compound(ir%nodes(k)%operation)) then
                    count_compounds = count_compounds - 1
                end if
            end if
        end do
    end function count_compounds

    subroutine append_arguments(b, spec, cuda)
        type(strbuf_t), intent(inout) :: b
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(in) :: cuda
        character(:), allocatable :: input_type, output_type
        integer :: k

        if (cuda) then
            if (spec%precision == PRECISION_REAL32 .or. &
                spec%precision == PRECISION_MIXED) then
                input_type = "const float "
            else
                input_type = "const double "
            end if
            if (spec%precision == PRECISION_MIXED) then
                output_type = "double* "
            else if (spec%precision == PRECISION_REAL32) then
                output_type = "float* "
            else
                output_type = "double* "
            end if
        else
            if (spec%precision == PRECISION_REAL32 .or. &
                spec%precision == PRECISION_MIXED) then
                input_type = "real(real32) "
            else
                input_type = "real(real64) "
            end if
            if (spec%precision == PRECISION_MIXED) then
                output_type = "real(real64) "
            else if (spec%precision == PRECISION_REAL32) then
                output_type = "real(real32) "
            else
                output_type = "real(real64) "
            end if
        end if

        do k = 1, size(spec%args)
            if (k > 1) call b%append(", ")
            if (cuda) call b%append(input_type)
            call b%append(chars(spec%args(k)))
        end do
        do k = 1, size(spec%outputs)
            if (size(spec%args) > 0 .or. k > 1) call b%append(", ")
            if (cuda) call b%append(output_type)
            call b%append(chars(spec%outputs(k)))
        end do
    end subroutine append_arguments

    subroutine append_declaration(b, declaration, names)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: declaration
        type(str_t), intent(in) :: names(:)
        integer :: k

        call b%append("    ")
        call b%append(declaration)
        call b%append(" :: ")
        do k = 1, size(names)
            if (k > 1) call b%append(", ")
            call b%append(chars(names(k)))
        end do
        call b%newline()
    end subroutine append_declaration

    subroutine append_temporary_declaration(b, prefix, ir, skip_nodes, precision)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: prefix
        type(kernel_ir_t), intent(in) :: ir
        logical, intent(in), optional :: skip_nodes(:)
        integer, intent(in) :: precision
        integer :: k
        logical :: first

        if (precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED) then
            call b%append("    real(real32) :: ")
        else
            call b%append("    real(real64) :: ")
        end if
        first = .true.
        do k = 1, ir%n_nodes
            if (.not. is_compound(ir%nodes(k)%operation)) cycle
            if (present(skip_nodes)) then
                if (skip_nodes(k)) cycle
            end if
            if (.not. first) call b%append(", ")
            call b%append(prefix//chars(str(k)))
            first = .false.
        end do
        call b%newline()
    end subroutine append_temporary_declaration

    subroutine append_provenance(b, spec, comment)
        type(strbuf_t), intent(inout) :: b
        type(kernel_emit_spec_t), intent(in) :: spec
        character(*), intent(in) :: comment

        call b%append(comment//" Generator: ")
        call b%append(chars(spec%generator))
        call b%newline()
        if (len(chars(spec%generator_revision)) > 0) then
            call b%append(comment//" Generator revision: ")
            call b%append(chars(spec%generator_revision))
            call b%newline()
        end if
        call b%append(comment//" Regenerate with: ")
        if (len(chars(spec%regenerate_command)) > 0) then
            call b%append(chars(spec%regenerate_command))
        else
            call b%append("fo exec ")
            call b%append(chars(spec%generator))
        end if
        call b%newline()
        call b%newline()
    end subroutine append_provenance

    function backend_name(backend) result(name)
        integer, intent(in) :: backend
        character(:), allocatable :: name

        if (backend == BACKEND_CUDA) then
            name = "CUDA"
        else if (backend == BACKEND_PYTHON) then
            name = "Python"
        else
            name = "Fortran"
        end if
    end function backend_name


end module fortgen_kernel_emit
