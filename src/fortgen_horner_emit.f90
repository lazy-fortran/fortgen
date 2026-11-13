module fortgen_horner_emit
    !! Fixed-degree tensor-product polynomial evaluation with fused partial
    !! derivatives. The polynomial is p(x) = sum_k c(k) prod_j x_j**k_j over
    !! 0 <= k_j <= degree(j), held per quantity in one contiguous block.
    !!
    !! The schedule is nested Horner, direction 1 innermost. Each direction
    !! carries a truncated Taylor jet q_a = (1/a!) d^a/dx^a, whose Horner step
    !! q_a <- q_(a-1) + x*q_a is one fused multiply-add per derivative, so no
    !! coefficient is ever scaled by k or k*(k-1). Only the needed jets are
    !! kept, structural zeros are removed at generation time and coefficients
    !! are read once, in a single streaming pass over the block. Outputs are
    !! the jets scaled by alpha!.
    use fortgen_string, only: str_t, str, chars
    use fortgen_buffer, only: buffer_t
    use fortgen_layout, only: wrap_fortran_source
    use fortgen_names, only: valid_fortran_name
    use fortgen_kernel_ir, only: TYPE_REAL32, TYPE_REAL64
    use fortgen_kernel_target, only: TARGET_FORTRAN_CPU, TARGET_FORTRAN_OPENACC, &
        TARGET_CUDA, TARGET_METAL
    use fortgen_derivative_set, only: derivative_set_t
    implicit none
    private
    public :: horner_kernel_spec_t, emit_horner_kernel, horner_derivative_count

    integer, parameter :: MAX_DIMENSION = 3
    integer, parameter :: MAX_DEGREE = 9

    type :: horner_kernel_spec_t
        type(str_t) :: name
        !> The emitted partials; its dimension (1..3) is the number of
        !> variables.
        type(derivative_set_t) :: derivatives
        !> Polynomial degree per direction, 0..9.
        integer :: degree(MAX_DIMENSION) = 0
        !> Quantities sharing the point. Zero makes nq a runtime argument.
        integer :: nq = 0
        !> TARGET_FORTRAN_CPU, TARGET_FORTRAN_OPENACC, TARGET_CUDA or
        !> TARGET_METAL.
        integer :: target = TARGET_FORTRAN_CPU
        !> Coefficient storage may be narrowed independently of the recurrence.
        integer :: coefficient_type = TYPE_REAL64
        !> Coordinates, accumulators and outputs: binary64, except binary32
        !> on Metal, which has no binary64 type.
        integer :: compute_type = TYPE_REAL64
    end type

    !> Generation state: requested and needed multi-indices plus the
    !> structural-zero flag of every jet accumulator.
    type :: plan_t
        integer :: d = 0
        integer :: n(MAX_DIMENSION) = 0
        integer :: amax(MAX_DIMENSION) = 0
        logical, allocatable :: wanted(:, :, :)
        logical, allocatable :: needed(:, :, :)
        logical, allocatable :: live(:, :, :, :)
        logical, allocatable :: ever(:, :, :, :)
        !> C-family syntax: CUDA or Metal.
        logical :: cuda = .false.
        logical :: metal = .false.
        integer :: nq = 0
        integer :: coefficient_type = TYPE_REAL64
    end type

contains

    !> Number of emitted derivatives of total order m.
    function horner_derivative_count(spec, m) result(count)
        type(horner_kernel_spec_t), intent(in) :: spec
        integer, intent(in) :: m
        integer :: count
        type(plan_t) :: p
        logical :: ok
        character(:), allocatable :: message
        integer :: a1, a2, a3

        count = 0
        call make_plan(spec, p, ok, message)
        if (.not. ok) return
        do a1 = 0, p%amax(1)
            do a2 = 0, p%amax(2)
                do a3 = 0, p%amax(3)
                    if (p%wanted(a1, a2, a3) .and. a1 + a2 + a3 == m) &
                        count = count + 1
                end do
            end do
        end do
    end function

    function emit_horner_kernel(spec, ok, message) result(source)
        type(horner_kernel_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(plan_t) :: p
        type(buffer_t) :: b

        source = str('')
        call make_plan(spec, p, ok, message)
        if (.not. ok) return
        if (p%metal) then
            call emit_metal(spec, p, b)
            source = str(b%str())
        else if (p%cuda) then
            call emit_cuda(spec, p, b)
            source = str(b%str())
        else
            call emit_fortran(spec, p, b)
            source = str(wrap_fortran_source(b%str()))
        end if
    end function

    subroutine make_plan(spec, p, ok, message)
        type(horner_kernel_spec_t), intent(in) :: spec
        type(plan_t), intent(out) :: p
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: j, i, a(MAX_DIMENSION), a1, a2, a3

        ok = .false.
        message = 'horner kernel: invalid name, dimension, degree or nq'
        if (.not. valid_fortran_name(chars(spec%name))) return
        p%d = spec%derivatives%dimension
        if (p%d < 1 .or. p%d > MAX_DIMENSION) return
        if (spec%nq < 0) return
        do j = 1, p%d
            if (spec%degree(j) < 0 .or. spec%degree(j) > MAX_DEGREE) return
        end do
        message = 'horner kernel: invalid target'
        if (spec%target /= TARGET_FORTRAN_CPU .and. &
            spec%target /= TARGET_FORTRAN_OPENACC .and. &
            spec%target /= TARGET_CUDA .and. spec%target /= TARGET_METAL) return
        message = 'horner kernel: unsupported coefficient or compute type'
        if (spec%coefficient_type /= TYPE_REAL32 .and. &
            spec%coefficient_type /= TYPE_REAL64) return
        if (spec%target == TARGET_METAL) then
            if (spec%compute_type /= TYPE_REAL32) return
            if (spec%coefficient_type /= TYPE_REAL32) return
        else if (spec%compute_type /= TYPE_REAL64) then
            return
        end if
        message = 'horner kernel: invalid derivative orders'
        if (.not. spec%derivatives%valid()) return
        if (any(spec%derivatives%orders > MAX_DEGREE)) return
        p%n = 0
        p%n(1:p%d) = spec%degree(1:p%d)
        p%nq = spec%nq
        p%coefficient_type = spec%coefficient_type
        p%metal = spec%target == TARGET_METAL
        p%cuda = spec%target == TARGET_CUDA .or. p%metal
        p%amax = 0
        p%amax(1:p%d) = maxval(spec%derivatives%orders, dim=2)
        allocate (p%wanted(0:p%amax(1), 0:p%amax(2), 0:p%amax(3)))
        allocate (p%needed(0:p%amax(1), 0:p%amax(2), 0:p%amax(3)))
        p%wanted = .false.
        do i = 1, spec%derivatives%count()
            a = 0
            a(1:p%d) = spec%derivatives%orders(:, i)
            p%wanted(a(1), a(2), a(3)) = .true.
        end do
        ! Downward closure: a jet entry feeds every entry above it.
        do a1 = 0, p%amax(1)
            do a2 = 0, p%amax(2)
                do a3 = 0, p%amax(3)
                    p%needed(a1, a2, a3) = any(p%wanted(a1:, a2:, a3:))
                end do
            end do
        end do
        allocate (p%live(0:p%amax(1), 0:p%amax(2), 0:p%amax(3), MAX_DIMENSION))
        p%live = .false.
        allocate (p%ever, mold=p%live)
        p%ever = .false.
        message = ''
        ok = .true.
    end subroutine

    ! Level-j accumulators hold the jets of the first j directions.
    logical function on_level(p, j, a1, a2, a3)
        type(plan_t), intent(in) :: p
        integer, intent(in) :: j, a1, a2, a3

        on_level = .false.
        if (j < 3 .and. a3 /= 0) return
        if (j < 2 .and. a2 /= 0) return
        if (j == 1) then
            on_level = any(p%needed(a1, :, :))
        else if (j == 2) then
            on_level = any(p%needed(a1, a2, :))
        else
            on_level = p%needed(a1, a2, a3)
        end if
    end function

    function acc_name(j, a1, a2, a3) result(s)
        integer, intent(in) :: j, a1, a2, a3
        character(:), allocatable :: s

        s = 's'//num(j)//'_'//num(a1)
        if (j >= 2) s = s//num(a2)
        if (j >= 3) s = s//num(a3)
    end function

    function num(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(16) :: tmp

        write (tmp, '(i0)') i
        s = trim(tmp)
    end function

    function xname(p, j) result(s)
        type(plan_t), intent(in) :: p
        integer, intent(in) :: j
        character(:), allocatable :: s

        if (p%d == 1) then
            s = 'x'
        else
            s = 'x'//num(j)
        end if
    end function

    function coeff_ref(p, k) result(s)
        type(plan_t), intent(in) :: p
        integer, intent(in) :: k(MAX_DIMENSION)
        character(:), allocatable :: s
        integer :: j, offset, stride

        if (p%cuda) then
            offset = 0
            stride = 1
            do j = 1, p%d
                offset = offset + k(j)*stride
                stride = stride*(p%n(j) + 1)
            end do
            if (p%nq == 0) then
                s = 'c[iq + nq*'//num(offset)//']'
            else
                s = 'c[iq + '//num(p%nq*offset)//']'
            end if
        else
            s = 'c(iq'
            do j = 1, p%d
                s = s//', '//num(k(j))
            end do
            s = s//')'
        end if
        if (p%coefficient_type == TYPE_REAL32 .and. .not. p%metal) then
            if (p%cuda) then
                s = 'static_cast<double>('//s//')'
            else
                s = 'real('//s//', real64)'
            end if
        end if
    end function

    subroutine assign(p, b, lhs, rhs)
        type(plan_t), intent(in) :: p
        type(buffer_t), intent(inout) :: b
        character(*), intent(in) :: lhs, rhs

        if (p%cuda) then
            call b%line('        '//lhs//' = '//rhs//';')
        else
            call b%line('        '//lhs//' = '//rhs)
        end if
    end subroutine

    subroutine horner_step(p, b, lhs, first, x)
        type(plan_t), intent(in) :: p
        type(buffer_t), intent(inout) :: b
        character(*), intent(in) :: lhs, first, x

        if (p%metal) then
            call b%line('        '//lhs//' = metal::fma('//x//', '//lhs//', '//first//');')
        else if (p%cuda) then
            call b%line('        '//lhs//' = fma('//x//', '//lhs//', '//first//');')
        else
            call b%line('        '//lhs//' = '//first//' + '//x//'*'//lhs)
        end if
    end subroutine

    !> One full Horner sweep along direction j for fixed higher indices k.
    !> On return the level-j accumulators hold the jets of the slice.
    recursive subroutine sweep(p, b, j, k)
        type(plan_t), intent(inout) :: p
        type(buffer_t), intent(inout) :: b
        integer, intent(in) :: j
        integer, intent(inout) :: k(MAX_DIMENSION)
        integer :: kj, a1, a2, a3, b1, b2, b3
        logical :: first_live, self_live
        character(:), allocatable :: first, lhs

        p%live(:, :, :, j) = .false.
        do kj = p%n(j), 0, -1
            k(j) = kj
            if (j > 1) call sweep(p, b, j - 1, k)
            ! Descending jet order, so each update reads the previous step.
            do a1 = p%amax(1), 0, -1
                do a2 = p%amax(2), 0, -1
                    do a3 = p%amax(3), 0, -1
                        if (.not. on_level(p, j, a1, a2, a3)) cycle
                        b1 = a1; b2 = a2; b3 = a3
                        select case (j)
                        case (1)
                            b1 = a1 - 1
                        case (2)
                            b2 = a2 - 1
                        case (3)
                            b3 = a3 - 1
                        end select
                        if (min(b1, b2, b3) >= 0) then
                            first_live = p%live(b1, b2, b3, j)
                            first = acc_name(j, b1, b2, b3)
                        else if (j == 1) then
                            first_live = .true.
                            first = coeff_ref(p, k)
                        else
                            first_live = p%live(a1, a2, a3, j - 1)
                            first = acc_name(j - 1, a1, a2, a3)
                        end if
                        self_live = p%live(a1, a2, a3, j)
                        lhs = acc_name(j, a1, a2, a3)
                        if (self_live .and. first_live) then
                            call horner_step(p, b, lhs, first, xname(p, j))
                        else if (self_live) then
                            call assign(p, b, lhs, xname(p, j)//'*'//lhs)
                        else if (first_live) then
                            call assign(p, b, lhs, first)
                        end if
                        p%live(a1, a2, a3, j) = self_live .or. first_live
                        p%ever(a1, a2, a3, j) = p%ever(a1, a2, a3, j) .or. &
                            p%live(a1, a2, a3, j)
                    end do
                end do
            end do
        end do
    end subroutine

    integer function factorial(n)
        integer, intent(in) :: n
        integer :: i

        factorial = 1
        do i = 2, n
            factorial = factorial*i
        end do
    end function

    !> Output targets in canonical order: total order, then descending
    !> lexicographic multi-index (11, 12, 13, 22, 23, 33 for a Hessian).
    subroutine put_outputs(p, b)
        type(plan_t), intent(in) :: p
        type(buffer_t), intent(inout) :: b
        integer :: m, a1, a2, a3, slot, nm, scale
        character(:), allocatable :: target, value

        do m = 0, sum(p%amax)
            nm = order_count(p, m)
            slot = 0
            do a1 = p%amax(1), 0, -1
                do a2 = p%amax(2), 0, -1
                    do a3 = p%amax(3), 0, -1
                        if (a1 + a2 + a3 /= m) cycle
                        if (.not. p%wanted(a1, a2, a3)) cycle
                        slot = slot + 1
                        target = output_ref(p, m, slot, nm)
                        if (p%live(a1, a2, a3, p%d)) then
                            value = acc_name(p%d, a1, a2, a3)
                            scale = factorial(a1)*factorial(a2)*factorial(a3)
                            if (scale /= 1) then
                                if (p%metal) then
                                    value = num(scale)//'.0f*'//value
                                else if (p%cuda) then
                                    value = num(scale)//'.0*'//value
                                else
                                    value = num(scale)//'.0_real64*'//value
                                end if
                            end if
                        else if (p%metal) then
                            value = '0.0f'
                        else if (p%cuda) then
                            value = '0.0'
                        else
                            value = '0.0_real64'
                        end if
                        call assign(p, b, target, value)
                    end do
                end do
            end do
        end do
    end subroutine

    integer function order_count(p, m)
        type(plan_t), intent(in) :: p
        integer, intent(in) :: m
        integer :: a1, a2, a3

        order_count = 0
        do a1 = 0, p%amax(1)
            do a2 = 0, p%amax(2)
                do a3 = 0, p%amax(3)
                    if (a1 + a2 + a3 == m .and. p%wanted(a1, a2, a3)) &
                        order_count = order_count + 1
                end do
            end do
        end do
    end function

    function output_name(m) result(s)
        integer, intent(in) :: m
        character(:), allocatable :: s

        select case (m)
        case (0)
            s = 'y'
        case (1)
            s = 'dy'
        case default
            s = 'd'//num(m)//'y'
        end select
    end function

    function output_ref(p, m, slot, nm) result(s)
        type(plan_t), intent(in) :: p
        integer, intent(in) :: m, slot, nm
        character(:), allocatable :: s

        if (p%cuda) then
            if (m == 0 .or. p%d == 1) then
                s = output_name(m)//'[iq]'
            else
                s = output_name(m)//'['//num(slot - 1)//' + '//num(nm)//'*iq]'
            end if
        else
            if (m == 0 .or. p%d == 1) then
                s = output_name(m)//'(iq)'
            else
                s = output_name(m)//'('//num(slot)//', iq)'
            end if
        end if
    end function

    subroutine declare_accumulators(p, b, prefix, suffix)
        type(plan_t), intent(in) :: p
        type(buffer_t), intent(inout) :: b
        character(*), intent(in) :: prefix, suffix
        integer :: j, a1, a2, a3
        character(:), allocatable :: names

        do j = 1, p%d
            names = ''
            do a1 = 0, p%amax(1)
                do a2 = 0, p%amax(2)
                    do a3 = 0, p%amax(3)
                        if (.not. p%ever(a1, a2, a3, j)) cycle
                        if (len(names) > 0) names = names//', '
                        names = names//acc_name(j, a1, a2, a3)
                    end do
                end do
            end do
            if (len(names) > 0) call b%line(prefix//names//suffix)
        end do
    end subroutine

    function nq_text(p) result(s)
        type(plan_t), intent(in) :: p
        character(:), allocatable :: s

        if (p%nq == 0) then
            s = 'nq'
        else
            s = num(p%nq)
        end if
    end function

    subroutine emit_fortran(spec, p, b)
        type(horner_kernel_spec_t), intent(in) :: spec
        type(plan_t), intent(inout) :: p
        type(buffer_t), intent(inout) :: b
        type(buffer_t) :: body
        character(:), allocatable :: args, shape
        integer :: j, m, nm, k(MAX_DIMENSION)

        call b%line('! Generated by FortGen Horner schedule. Do not edit.')
        args = ''
        if (p%nq == 0) args = 'nq, '
        args = args//'c'
        do j = 1, p%d
            args = args//', '//xname(p, j)
        end do
        do m = 0, sum(p%amax)
            if (order_count(p, m) > 0) args = args//', '//output_name(m)
        end do
        call b%line('pure subroutine '//chars(spec%name)//'('//args//')')
        if (p%coefficient_type == TYPE_REAL32) then
            call b%line('    use, intrinsic :: iso_fortran_env, only: real32, real64')
        else
            call b%line('    use, intrinsic :: iso_fortran_env, only: real64')
        end if
        call b%line('    implicit none')
        if (p%nq == 0) call b%line('    integer, intent(in) :: nq')
        shape = nq_text(p)
        do j = 1, p%d
            shape = shape//', 0:'//num(p%n(j))
        end do
        if (p%coefficient_type == TYPE_REAL32) then
            call b%line('    real(real32), intent(in) :: c('//shape//')')
        else
            call b%line('    real(real64), intent(in) :: c('//shape//')')
        end if
        do j = 1, p%d
            call b%line('    real(real64), intent(in) :: '//xname(p, j))
        end do
        do m = 0, sum(p%amax)
            nm = order_count(p, m)
            if (nm == 0) cycle
            if (m == 0 .or. p%d == 1) then
                shape = nq_text(p)
            else
                shape = num(nm)//', '//nq_text(p)
            end if
            call b%line('    real(real64), intent(out) :: '//output_name(m)// &
                '('//shape//')')
        end do
        k = 0
        call sweep(p, body, p%d, k)
        call put_outputs(p, body)
        call declare_accumulators(p, b, '    real(real64) :: ', '')
        call b%line('    integer :: iq')
        if (spec%target == TARGET_FORTRAN_OPENACC) call b%line('    !$acc routine seq')
        call b%line('')
        call b%line('    do iq = 1, '//nq_text(p))
        call b%put(body%str())
        call b%line('    end do')
        call b%line('end subroutine '//chars(spec%name))
    end subroutine

    subroutine emit_cuda(spec, p, b)
        type(horner_kernel_spec_t), intent(in) :: spec
        type(plan_t), intent(inout) :: p
        type(buffer_t), intent(inout) :: b
        type(buffer_t) :: body
        character(:), allocatable :: args
        integer :: j, m, k(MAX_DIMENSION)

        call b%line('/* Generated by FortGen Horner schedule. Do not edit. */')
        args = ''
        if (p%nq == 0) args = 'int nq, '
        if (p%coefficient_type == TYPE_REAL32) then
            args = args//'const float* __restrict__ c'
        else
            args = args//'const double* __restrict__ c'
        end if
        do j = 1, p%d
            args = args//', double '//xname(p, j)
        end do
        do m = 0, sum(p%amax)
            if (order_count(p, m) > 0) &
                args = args//', double* __restrict__ '//output_name(m)
        end do
        call b%line('extern "C" __device__ __forceinline__')
        call b%line('void '//chars(spec%name)//'('//args//')')
        call b%line('{')
        k = 0
        call sweep(p, body, p%d, k)
        call put_outputs(p, body)
        call b%line('    for (int iq = 0; iq < '//nq_text(p)//'; ++iq) {')
        call declare_accumulators(p, b, '        double ', ';')
        call b%put(body%str())
        call b%line('    }')
        call b%line('}')
    end subroutine

    !> A Metal device function in binary32 with thread-address-space outputs,
    !> the convention of FortGen's Metal scalar leaves.
    subroutine emit_metal(spec, p, b)
        type(horner_kernel_spec_t), intent(in) :: spec
        type(plan_t), intent(inout) :: p
        type(buffer_t), intent(inout) :: b
        type(buffer_t) :: body
        character(:), allocatable :: args
        integer :: j, m, k(MAX_DIMENSION)

        call b%line('/* Generated by FortGen Horner schedule. Do not edit. */')
        args = ''
        if (p%nq == 0) args = 'int nq, '
        args = args//'device const float* c'
        do j = 1, p%d
            args = args//', float '//xname(p, j)
        end do
        do m = 0, sum(p%amax)
            if (order_count(p, m) > 0) args = args//', thread float* '//output_name(m)
        end do
        call b%line('inline void '//chars(spec%name)//'('//args//')')
        call b%line('{')
        k = 0
        call sweep(p, body, p%d, k)
        call put_outputs(p, body)
        call b%line('    for (int iq = 0; iq < '//nq_text(p)//'; ++iq) {')
        call declare_accumulators(p, b, '        float ', ';')
        call b%put(body%str())
        call b%line('    }')
        call b%line('}')
    end subroutine

end module fortgen_horner_emit
