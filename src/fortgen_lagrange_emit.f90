module fortgen_lagrange_emit
    !! Tensor-product Lagrange interpolation at one point from nodal values on
    !! a uniform stencil, with the optional gradient. The stencil of direction
    !! j has n_j nodes at t_j = 0, 1, .., n_j - 1 in units of its mesh step.
    !!
    !! The weights of each direction are straight-line prefix and suffix
    !! products of (t - k) with the integer node denominators folded into
    !! constants, so no division depends on the point. The nodal block is then
    !! contracted one direction at a time (sum factorisation): direction 1
    !! first, then 2, then 3, carrying the value and one derivative channel
    !! per direction, which costs O(n**d) per quantity instead of O(d n**d)
    !! weight products per node.
    use fortgen_string, only: str_t, str, chars
    use fortgen_buffer, only: buffer_t
    use fortgen_layout, only: wrap_fortran_source
    use fortgen_names, only: valid_fortran_name
    use fortgen_kernel_target, only: TARGET_FORTRAN_CPU, TARGET_FORTRAN_OPENACC
    implicit none
    private
    public :: lagrange_kernel_spec_t, emit_lagrange_kernel

    integer, parameter :: MAX_DIMENSION = 3
    integer, parameter :: MAX_POINTS = 10

    type :: lagrange_kernel_spec_t
        type(str_t) :: name
        !> Number of variables, 1..3.
        integer :: dimension = 1
        !> Stencil nodes per direction, 2..10.
        integer :: points(MAX_DIMENSION) = 0
        !> Also emit the gradient with respect to the physical coordinates.
        logical :: gradient = .true.
        !> Quantities sharing the point. Zero makes nq a runtime argument.
        integer :: nq = 0
        !> TARGET_FORTRAN_CPU or TARGET_FORTRAN_OPENACC.
        integer :: target = TARGET_FORTRAN_CPU
    end type

contains

    !> Fortran source of the kernel, or empty source with ok = .false. and a
    !> diagnostic for an invalid spec. The emitted leaf is
    !>   pure subroutine NAME([nq,] f, t1[, t2, t3], h1[, h2, h3], y[, dy])
    !> with f(nq, n1[, n2, n3]) the nodal values, t_j the point in mesh steps
    !> from the first stencil node, h_j the reciprocal mesh steps (they scale
    !> only the gradient), y(nq) the value and dy(d, nq) the gradient.
    function emit_lagrange_kernel(spec, ok, message) result(source)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(buffer_t) :: b

        source = str('')
        call check(spec, ok, message)
        if (.not. ok) return
        call emit_fortran(spec, b)
        source = str(wrap_fortran_source(b%str()))
    end function

    subroutine check(spec, ok, message)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        ok = .false.
        if (.not. valid_fortran_name(chars(spec%name))) then
            message = 'invalid kernel name'
        else if (spec%dimension < 1 .or. spec%dimension > MAX_DIMENSION) then
            message = 'dimension must be 1, 2 or 3'
        else if (any(spec%points(1:spec%dimension) < 2) .or. &
                 any(spec%points(1:spec%dimension) > MAX_POINTS)) then
            message = 'points per direction must be 2..10'
        else if (spec%nq < 0) then
            message = 'nq must be zero (runtime) or positive'
        else if (spec%target /= TARGET_FORTRAN_CPU .and. &
                 spec%target /= TARGET_FORTRAN_OPENACC) then
            message = 'target must be TARGET_FORTRAN_CPU or TARGET_FORTRAN_OPENACC'
        else
            ok = .true.
            message = ''
        end if
    end subroutine

    subroutine emit_fortran(spec, b)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        type(buffer_t), intent(inout) :: b
        integer :: d, j
        character(:), allocatable :: args, nqs, fdims

        d = spec%dimension
        nqs = nq_text(spec)
        args = ''
        if (spec%nq == 0) args = 'nq, '
        args = args//'f'
        do j = 1, d
            args = args//', t'//itoa(j)
        end do
        do j = 1, d
            args = args//', h'//itoa(j)
        end do
        args = args//', y'
        if (spec%gradient) args = args//', dy'
        fdims = nqs
        do j = 1, d
            fdims = fdims//', '//itoa(spec%points(j))
        end do

        call b%line('pure subroutine '//chars(spec%name)//'('//args//')')
        call b%line('use, intrinsic :: iso_fortran_env, only: real64')
        call b%line('implicit none')
        if (spec%nq == 0) call b%line('integer, intent(in) :: nq')
        call b%line('real(real64), intent(in) :: f('//fdims//')')
        do j = 1, d
            call b%line('real(real64), intent(in) :: t'//itoa(j)//', h'//itoa(j))
        end do
        call b%line('real(real64), intent(out) :: y('//nqs//')')
        if (spec%gradient) call b%line('real(real64), intent(out) :: dy('// &
                                       itoa(d)//', '//nqs//')')
        do j = 1, d
            call declare_weights(spec, j, b)
        end do
        call declare_contraction(spec, b)
        if (spec%target == TARGET_FORTRAN_OPENACC) call b%line('!$acc routine seq')
        do j = 1, d
            call emit_weights(spec, j, b)
        end do
        call emit_contraction(spec, b)
        call b%line('end subroutine '//chars(spec%name))
    end subroutine

    subroutine declare_weights(spec, j, b)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        integer, intent(in) :: j
        type(buffer_t), intent(inout) :: b
        character(:), allocatable :: n

        n = itoa(spec%points(j))
        call b%line('real(real64) :: w'//itoa(j)//'('//n//'), p'//itoa(j)//'('//n// &
                    '), s'//itoa(j)//'('//n//')')
        if (spec%gradient) call b%line('real(real64) :: dw'//itoa(j)//'('//n// &
                                       '), dp'//itoa(j)//'('//n//'), ds'//itoa(j)//'('//n//')')
    end subroutine

    !> Straight-line prefix products p(i) = prod_{k < i} (t - k + 1), suffix
    !> products s(i) = prod_{k > i} (t - k + 1), and their t derivatives.
    subroutine emit_weights(spec, j, b)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        integer, intent(in) :: j
        type(buffer_t), intent(inout) :: b
        character(:), allocatable :: c, t
        integer :: n, i

        n = spec%points(j)
        c = itoa(j)
        t = 't'//c
        call b%line('p'//c//'(1) = 1.0_real64')
        if (spec%gradient) call b%line('dp'//c//'(1) = 0.0_real64')
        do i = 2, n
            call b%line('p'//c//'('//itoa(i)//') = p'//c//'('//itoa(i - 1)//')*('// &
                        shifted(t, i - 2)//')')
            if (spec%gradient) call b%line('dp'//c//'('//itoa(i)//') = dp'//c//'('// &
                itoa(i - 1)//')*('//shifted(t, i - 2)//') + p'//c//'('//itoa(i - 1)//')')
        end do
        call b%line('s'//c//'('//itoa(n)//') = 1.0_real64')
        if (spec%gradient) call b%line('ds'//c//'('//itoa(n)//') = 0.0_real64')
        do i = n - 1, 1, -1
            call b%line('s'//c//'('//itoa(i)//') = s'//c//'('//itoa(i + 1)//')*('// &
                        shifted(t, i)//')')
            if (spec%gradient) call b%line('ds'//c//'('//itoa(i)//') = ds'//c//'('// &
                itoa(i + 1)//')*('//shifted(t, i)//') + s'//c//'('//itoa(i + 1)//')')
        end do
        do i = 1, n
            call b%line('w'//c//'('//itoa(i)//') = p'//c//'('//itoa(i)//')*s'//c//'('// &
                        itoa(i)//')*'//reciprocal(denominator(n, i)))
            if (spec%gradient) call b%line('dw'//c//'('//itoa(i)//') = (dp'//c//'('// &
                itoa(i)//')*s'//c//'('//itoa(i)//') + p'//c//'('//itoa(i)//')*ds'//c// &
                '('//itoa(i)//'))*('//reciprocal(denominator(n, i))//'*h'//c//')')
        end do
    end subroutine

    subroutine declare_contraction(spec, b)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        type(buffer_t), intent(inout) :: b
        character(:), allocatable :: n2, n3

        call b%line('integer :: iq')
        select case (spec%dimension)
        case (2)
            n2 = itoa(spec%points(2))
            call b%line('integer :: i2')
            call b%line('real(real64) :: v('//n2//')')
            if (spec%gradient) call b%line('real(real64) :: vx('//n2//')')
        case (3)
            n2 = itoa(spec%points(2))
            n3 = itoa(spec%points(3))
            call b%line('integer :: i2, i3')
            call b%line('real(real64) :: v('//n2//', '//n3//'), g('//n3//')')
            if (spec%gradient) call b%line('real(real64) :: vx('//n2//', '//n3// &
                                           '), gx('//n3//'), gy('//n3//')')
        end select
    end subroutine

    subroutine emit_contraction(spec, b)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        type(buffer_t), intent(inout) :: b
        character(:), allocatable :: q

        q = nq_text(spec)
        call b%line('do iq = 1, '//q)
        select case (spec%dimension)
        case (1)
            call b%line('y(iq) = dot_product(f(iq, :), w1)')
            if (spec%gradient) call b%line('dy(1, iq) = dot_product(f(iq, :), dw1)')
        case (2)
            call b%line('do i2 = 1, '//itoa(spec%points(2)))
            call b%line('v(i2) = dot_product(f(iq, :, i2), w1)')
            if (spec%gradient) call b%line('vx(i2) = dot_product(f(iq, :, i2), dw1)')
            call b%line('end do')
            call b%line('y(iq) = dot_product(v, w2)')
            if (spec%gradient) then
                call b%line('dy(1, iq) = dot_product(vx, w2)')
                call b%line('dy(2, iq) = dot_product(v, dw2)')
            end if
        case (3)
            call b%line('do i3 = 1, '//itoa(spec%points(3)))
            call b%line('do i2 = 1, '//itoa(spec%points(2)))
            call b%line('v(i2, i3) = dot_product(f(iq, :, i2, i3), w1)')
            if (spec%gradient) call b%line('vx(i2, i3) = dot_product(f(iq, :, i2, i3), dw1)')
            call b%line('end do')
            call b%line('g(i3) = dot_product(v(:, i3), w2)')
            if (spec%gradient) then
                call b%line('gx(i3) = dot_product(vx(:, i3), w2)')
                call b%line('gy(i3) = dot_product(v(:, i3), dw2)')
            end if
            call b%line('end do')
            call b%line('y(iq) = dot_product(g, w3)')
            if (spec%gradient) then
                call b%line('dy(1, iq) = dot_product(gx, w3)')
                call b%line('dy(2, iq) = dot_product(gy, w3)')
                call b%line('dy(3, iq) = dot_product(g, dw3)')
            end if
        end select
        call b%line('end do')
    end subroutine

    function nq_text(spec) result(s)
        type(lagrange_kernel_spec_t), intent(in) :: spec
        character(:), allocatable :: s

        if (spec%nq == 0) then
            s = 'nq'
        else
            s = itoa(spec%nq)
        end if
    end function

    !> prod_{k /= i} (i - k) over the nodes 1..n.
    pure integer function denominator(n, i) result(den)
        integer, intent(in) :: n, i
        integer :: k

        den = 1
        do k = 1, n
            if (k /= i) den = den*(i - k)
        end do
    end function

    !> Exact reciprocal of an integer as a Fortran expression.
    function reciprocal(den) result(s)
        integer, intent(in) :: den
        character(:), allocatable :: s

        if (den == 1) then
            s = '1.0_real64'
        else if (den == -1) then
            s = '(-1.0_real64)'
        else if (den > 0) then
            s = '(1.0_real64/'//itoa(den)//'.0_real64)'
        else
            s = '(-1.0_real64/'//itoa(-den)//'.0_real64)'
        end if
    end function

    function shifted(t, k) result(s)
        character(*), intent(in) :: t
        integer, intent(in) :: k
        character(:), allocatable :: s

        if (k == 0) then
            s = t
        else
            s = t//' - '//itoa(k)//'.0_real64'
        end if
    end function

    function itoa(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(16) :: buf

        write (buf, '(i0)') i
        s = trim(buf)
    end function
end module fortgen_lagrange_emit
