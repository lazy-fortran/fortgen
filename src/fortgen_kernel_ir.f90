module fortgen_kernel_ir
    !! Stable backend-neutral scalar kernel IR shared by all FortGen frontends.
    !!
    !! Nodes are stored in topological order. Operands contain 1-based node
    !! indices; each node owns a contiguous slice described by first_operand and
    !! n_operands. outputs contains 1-based root node indices.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortgen_string, only: str_t
    implicit none
    private

    public :: kernel_ir_t, kernel_ir_node_t
    public :: IR_LITERAL, IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, IR_FUNCTION
    public :: kernel_ir_allocate, kernel_ir_validate_structure

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

end module fortgen_kernel_ir
