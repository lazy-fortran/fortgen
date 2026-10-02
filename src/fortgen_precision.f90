module fortgen_precision
    ! Precision choices for generated scalar kernels.
    implicit none
    private

    public :: PRECISION_REAL64, PRECISION_REAL32, PRECISION_MIXED
    public :: precision_is_valid, precision_name, precision_from_name

    integer, parameter :: PRECISION_REAL64 = 1
    integer, parameter :: PRECISION_REAL32 = 2
    integer, parameter :: PRECISION_MIXED = 3

contains

    pure function precision_is_valid(precision) result(valid)
        integer, intent(in) :: precision
        logical :: valid

        valid = precision == PRECISION_REAL64 .or. &
            precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED
    end function precision_is_valid

    pure function precision_name(precision) result(name)
        integer, intent(in) :: precision
        character(:), allocatable :: name

        select case (precision)
        case (PRECISION_REAL64)
            name = "real64"
        case (PRECISION_REAL32)
            name = "real32"
        case (PRECISION_MIXED)
            name = "mixed"
        case default
            name = "invalid"
        end select
    end function precision_name

    pure integer function precision_from_name(name) result(precision)
        character(*), intent(in) :: name
        select case (trim(name))
        case ("real64"); precision = PRECISION_REAL64
        case ("real32"); precision = PRECISION_REAL32
        case ("mixed");  precision = PRECISION_MIXED
        case default;    precision = 0
        end select
    end function precision_from_name

end module fortgen_precision
