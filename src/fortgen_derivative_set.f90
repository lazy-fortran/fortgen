module fortgen_derivative_set
    !! Which partial derivatives a kernel produces, as data. The Horner
    !! emitter and FortSym's symbolic jets read the same set, so a spline leaf
    !! and the symbolic chain that consumes it select the same partials by
    !! construction.
    !!
    !! A set holds distinct multi-indices alpha(1:dimension), alpha(j) being
    !! the derivative count along variable j, in canonical order: total order
    !! ascending, then descending lexicographic. For three variables that is
    !! 000; 100, 010, 001; 200, 110, 101, 020, 011, 002, the order of a packed
    !! symmetric Hessian.
    implicit none
    private
    public :: derivative_set_t, partials, radial_mixed, multi_indices

    type :: derivative_set_t
        integer :: dimension = 0
        integer, allocatable :: orders(:, :)
    contains
        procedure :: count => set_count
        procedure :: of_order => set_of_order
        procedure :: touching => set_touching
        procedure :: without => set_without
        procedure :: valid => set_valid
    end type

contains

    !> Every partial derivative of total order 0..max_order.
    function partials(dimension, max_order) result(set)
        integer, intent(in) :: dimension, max_order
        type(derivative_set_t) :: set
        integer :: m

        set%dimension = dimension
        allocate (set%orders(max(dimension, 0), 0))
        if (dimension < 1 .or. max_order < 0) return
        do m = 0, max_order
            call append_order(set, m)
        end do
    end function

    !> Value, gradient and the second derivatives with an index along
    !> variable 1, the radial direction of a flux-coordinate spline.
    function radial_mixed(dimension) result(set)
        integer, intent(in) :: dimension
        type(derivative_set_t) :: set
        type(derivative_set_t) :: second

        set = partials(dimension, 1)
        second = partials(dimension, 2)
        second = second%of_order(2)
        second = second%touching([1])
        set%orders = reshape([set%orders, second%orders], &
            [dimension, set%count() + second%count()])
    end function

    !> An explicit set; the multi-indices are put into canonical order.
    !> Duplicates are kept, so that valid() can refuse them.
    function multi_indices(orders) result(set)
        integer, intent(in) :: orders(:, :)
        type(derivative_set_t) :: set
        integer :: rank(size(orders, 2)), i, j, key

        set%dimension = size(orders, 1)
        do i = 1, size(orders, 2)
            rank(i) = 1
            do j = 1, size(orders, 2)
                if (j == i) cycle
                key = compare(orders(:, j), orders(:, i))
                if (key < 0 .or. (key == 0 .and. j < i)) rank(i) = rank(i) + 1
            end do
        end do
        allocate (set%orders(size(orders, 1), size(orders, 2)))
        do i = 1, size(orders, 2)
            set%orders(:, rank(i)) = orders(:, i)
        end do
    end function

    integer function set_count(self)
        class(derivative_set_t), intent(in) :: self

        set_count = 0
        if (allocated(self%orders)) set_count = size(self%orders, 2)
    end function

    !> The members of total order m.
    function set_of_order(self, m) result(subset)
        class(derivative_set_t), intent(in) :: self
        integer, intent(in) :: m
        type(derivative_set_t) :: subset

        subset = select(self, sum(self%orders, 1) == m)
    end function

    !> The members that differentiate along at least one of `axes`.
    function set_touching(self, axes) result(subset)
        class(derivative_set_t), intent(in) :: self
        integer, intent(in) :: axes(:)
        type(derivative_set_t) :: subset
        logical, allocatable :: keep(:)
        integer :: i

        allocate (keep(self%count()))
        do i = 1, self%count()
            keep(i) = any(self%orders(axes, i) > 0)
        end do
        subset = select(self, keep)
    end function

    !> The set without the multi-index alpha.
    function set_without(self, alpha) result(subset)
        class(derivative_set_t), intent(in) :: self
        integer, intent(in) :: alpha(:)
        type(derivative_set_t) :: subset
        logical, allocatable :: keep(:)
        integer :: i

        allocate (keep(self%count()))
        do i = 1, self%count()
            keep(i) = any(self%orders(:, i) /= alpha)
        end do
        subset = select(self, keep)
    end function

    !> Nonempty, nonnegative and free of duplicates.
    logical function set_valid(self)
        class(derivative_set_t), intent(in) :: self
        integer :: i

        set_valid = .false.
        if (self%dimension < 1 .or. self%count() < 1) return
        if (size(self%orders, 1) /= self%dimension) return
        if (any(self%orders < 0)) return
        do i = 2, self%count()
            if (compare(self%orders(:, i - 1), self%orders(:, i)) >= 0) return
        end do
        set_valid = .true.
    end function

    function select(set, keep) result(subset)
        type(derivative_set_t), intent(in) :: set
        logical, intent(in) :: keep(:)
        type(derivative_set_t) :: subset
        integer :: i, n

        subset%dimension = set%dimension
        allocate (subset%orders(set%dimension, count(keep)))
        n = 0
        do i = 1, size(keep)
            if (.not. keep(i)) cycle
            n = n + 1
            subset%orders(:, n) = set%orders(:, i)
        end do
    end function

    !> Append every multi-index of total order m in descending lexicographic
    !> order.
    subroutine append_order(set, m)
        type(derivative_set_t), intent(inout) :: set
        integer, intent(in) :: m
        integer :: alpha(set%dimension)

        alpha = 0
        call fill(1, m)

    contains

        recursive subroutine fill(j, left)
            integer, intent(in) :: j, left
            integer :: a

            if (j == set%dimension) then
                alpha(j) = left
                set%orders = reshape([set%orders, alpha], &
                    [set%dimension, set%count() + 1])
                return
            end if
            do a = left, 0, -1
                alpha(j) = a
                call fill(j + 1, left - a)
            end do
        end subroutine

    end subroutine

    !> Sign of the canonical comparison of two multi-indices.
    integer function compare(a, b)
        integer, intent(in) :: a(:), b(:)
        integer :: j

        compare = sum(a) - sum(b)
        if (compare /= 0) then
            compare = sign(1, compare)
            return
        end if
        do j = 1, size(a)
            if (a(j) /= b(j)) then
                compare = merge(-1, 1, a(j) > b(j))
                return
            end if
        end do
    end function

end module fortgen_derivative_set
