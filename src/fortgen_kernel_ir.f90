module fortgen_kernel_ir
    !! Stable backend-neutral scalar kernel IR shared by all FortGen frontends.
    !!
    !! Nodes are stored in topological order. Operands contain 1-based node
    !! indices; each node owns a contiguous slice described by first_operand and
    !! n_operands. outputs contains 1-based root node indices.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortgen_string, only: str_t
    implicit none
    private

    public :: kernel_ir_t, kernel_ir_node_t
    public :: IR_LITERAL, IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, IR_FUNCTION
    public :: kernel_ir_allocate, kernel_ir_validate_structure

    public :: TYPE_DEFAULT, TYPE_REAL32, TYPE_UINT32, TYPE_UINT64, TYPE_BOOL
    public :: IR_SUB, IR_SHL, IR_SHR, IR_BITAND, IR_BITOR, IR_LT, IR_LE
    public :: IR_EQ, IR_NE, IR_GE, IR_GT, IR_AND, IR_OR, IR_NOT
    public :: IR_SELECT, IR_CAST, IR_MULHI, IR_DIV
    public :: kernel_ir_is_typed, kernel_ir_validate_types

    integer, parameter :: TYPE_DEFAULT = 0, TYPE_REAL32 = 1
    integer, parameter :: TYPE_UINT32 = 2, TYPE_UINT64 = 3, TYPE_BOOL = 4
    integer, parameter :: IR_SUB = 8, IR_SHL = 9, IR_SHR = 10
    integer, parameter :: IR_BITAND = 11, IR_BITOR = 12
    integer, parameter :: IR_LT = 13, IR_LE = 14, IR_EQ = 15, IR_NE = 16
    integer, parameter :: IR_GE = 17, IR_GT = 18, IR_AND = 19, IR_OR = 20
    integer, parameter :: IR_NOT = 21, IR_SELECT = 22, IR_CAST = 23
    integer, parameter :: IR_MULHI = 24, IR_DIV = 25

    integer, parameter :: dp = real64
    integer, parameter :: IR_LITERAL = 1
    integer, parameter :: IR_SYMBOL = 2
    integer, parameter :: IR_CONSTANT = 3
    integer, parameter :: IR_ADD = 4
    integer, parameter :: IR_MUL = 5
    integer, parameter :: IR_POW = 6
    integer, parameter :: IR_FUNCTION = 7

    type :: kernel_ir_node_t
        integer :: operation = 0
        integer :: first_operand = 0
        integer :: n_operands = 0
        real(dp) :: value = 0.0_dp
        type(str_t) :: name
        integer :: scalar_type = TYPE_DEFAULT
        ! Exact unsigned payload, high then low base-2**32 digits.
        integer(int64) :: uint_hi = 0_int64, uint_lo = 0_int64
        logical :: bool_value = .false.
        integer :: shift = 0
    end type kernel_ir_node_t

    type :: kernel_ir_t
        type(kernel_ir_node_t), allocatable :: nodes(:)
        integer, allocatable :: operands(:)
        integer, allocatable :: outputs(:)
        integer :: n_nodes = 0
        integer :: n_operands = 0
    contains
        procedure :: clear => kernel_ir_clear
    end type kernel_ir_t

contains

    subroutine kernel_ir_clear(self)
        class(kernel_ir_t), intent(inout) :: self
        if (allocated(self%nodes)) deallocate(self%nodes)
        if (allocated(self%operands)) deallocate(self%operands)
        if (allocated(self%outputs)) deallocate(self%outputs)
        self%n_nodes = 0
        self%n_operands = 0
    end subroutine kernel_ir_clear

    subroutine kernel_ir_allocate(ir, n_nodes, n_operands, n_outputs)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(in) :: n_nodes, n_operands, n_outputs
        call ir%clear()
        allocate(ir%nodes(n_nodes))
        allocate(ir%operands(n_operands))
        allocate(ir%outputs(n_outputs))
        ir%n_nodes = n_nodes
        ir%n_operands = n_operands
    end subroutine kernel_ir_allocate

    subroutine kernel_ir_validate_structure(ir, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: i, j, first, last, operand

        ok = .false.
        message = ""
        if (.not. allocated(ir%nodes) .or. .not. allocated(ir%operands) .or. &
            .not. allocated(ir%outputs)) then
            message = "kernel IR: storage is not allocated"
            return
        end if
        if (ir%n_nodes /= size(ir%nodes) .or. ir%n_operands /= size(ir%operands)) then
            message = "kernel IR: counts do not match storage"
            return
        end if
        do i = 1, ir%n_nodes
            if (ir%nodes(i)%n_operands < 0) then
                message = "kernel IR: negative operand count"
                return
            end if
            first = ir%nodes(i)%first_operand
            last = first + ir%nodes(i)%n_operands - 1
            if (ir%nodes(i)%n_operands > 0) then
                if (first < 1 .or. last > ir%n_operands) then
                    message = "kernel IR: operand slice is invalid"
                    return
                end if
                do j = first, last
                    operand = ir%operands(j)
                    if (operand < 1 .or. operand >= i) then
                        message = "kernel IR: nodes are not topological"
                        return
                    end if
                end do
            end if
        end do
        do i = 1, size(ir%outputs)
            if (ir%outputs(i) < 1 .or. ir%outputs(i) > ir%n_nodes) then
                message = "kernel IR: output root is invalid"
                return
            end if
        end do
        ok = .true.
    end subroutine kernel_ir_validate_structure

    logical function kernel_ir_is_typed(ir) result(typed)
        type(kernel_ir_t), intent(in) :: ir
        typed = .false.
        if (.not. allocated(ir%nodes)) return
        typed = any(ir%nodes%scalar_type /= TYPE_DEFAULT) .or. &
            any(ir%nodes%uint_hi /= 0) .or. any(ir%nodes%uint_lo /= 0) .or. &
            any(ir%nodes%bool_value) .or. any(ir%nodes%shift /= 0) .or. &
            any(ir%nodes%operation >= IR_SUB)
    end function kernel_ir_is_typed

    subroutine kernel_ir_validate_types(ir, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: i, j, t, a, n, first, width, op
        logical :: unsigned
        integer(int64), parameter :: max_digit = 4294967295_int64

        call kernel_ir_validate_structure(ir, ok, message)
        if (.not. ok) return
        if (.not. kernel_ir_is_typed(ir)) return
        ok = .false.
        do i = 1, ir%n_nodes
            t = ir%nodes(i)%scalar_type
            op = ir%nodes(i)%operation
            n = ir%nodes(i)%n_operands
            first = ir%nodes(i)%first_operand
            message = "kernel IR: invalid scalar type"
            if (t < TYPE_REAL32 .or. t > TYPE_BOOL) return
            unsigned = t == TYPE_UINT32 .or. t == TYPE_UINT64
            message = "kernel IR: unexpected unsigned payload"
            if (op /= IR_LITERAL .or. .not. unsigned) then
                if (ir%nodes(i)%uint_hi /= 0 .or. ir%nodes(i)%uint_lo /= 0) return
            end if
            message = "kernel IR: invalid typed arity or operand type"
            select case (op)
            case (IR_LITERAL, IR_SYMBOL)
                if (n /= 0) return
                if (op == IR_LITERAL .and. unsigned) then
                    message = "kernel IR: unsigned literal digit out of range"
                    if (ir%nodes(i)%uint_hi < 0 .or. &
                        ir%nodes(i)%uint_hi > max_digit) return
                    if (ir%nodes(i)%uint_lo < 0 .or. &
                        ir%nodes(i)%uint_lo > max_digit) return
                    if (t == TYPE_UINT32 .and. ir%nodes(i)%uint_hi /= 0) return
                end if
            case (IR_ADD, IR_MUL, IR_SUB, IR_DIV, IR_BITAND, IR_BITOR, IR_MULHI)
                if (n /= 2 .or. t == TYPE_BOOL) return
                if (op == IR_DIV .and. t /= TYPE_REAL32) return
                if (op >= IR_BITAND .and. op /= IR_DIV .and. .not. unsigned) return
                do j = first, first + n - 1
                    if (ir%nodes(ir%operands(j))%scalar_type /= t) return
                end do
            case (IR_SHL, IR_SHR)
                if (n /= 1 .or. .not. unsigned) return
                if (ir%nodes(ir%operands(first))%scalar_type /= t) return
                width = 32
                if (t == TYPE_UINT64) width = 64
                message = "kernel IR: literal shift count outside scalar width"
                if (ir%nodes(i)%shift < 0 .or. ir%nodes(i)%shift >= width) return
            case (IR_LT:IR_GT)
                if (n /= 2 .or. t /= TYPE_BOOL) return
                a = ir%nodes(ir%operands(first))%scalar_type
                if (ir%nodes(ir%operands(first + 1))%scalar_type /= a) return
                if (a == TYPE_BOOL .and. op /= IR_EQ .and. op /= IR_NE) return
            case (IR_AND, IR_OR, IR_NOT)
                a = 2
                if (op == IR_NOT) a = 1
                if (n /= a .or. t /= TYPE_BOOL) return
                do j = first, first + n - 1
                    if (ir%nodes(ir%operands(j))%scalar_type /= TYPE_BOOL) return
                end do
            case (IR_SELECT)
                if (n /= 3) return
                if (ir%nodes(ir%operands(first))%scalar_type /= TYPE_BOOL) return
                if (ir%nodes(ir%operands(first + 1))%scalar_type /= t) return
                if (ir%nodes(ir%operands(first + 2))%scalar_type /= t) return
            case (IR_CAST)
                if (n /= 1) return
                a = ir%nodes(ir%operands(first))%scalar_type
                if (a == TYPE_BOOL .or. t == TYPE_BOOL) return
            case default
                message = "kernel IR: unsupported typed operation"
                return
            end select
        end do
        ok = .true.
        message = ""
    end subroutine kernel_ir_validate_types

end module fortgen_kernel_ir
