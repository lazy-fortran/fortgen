program test_reciprocal_kernel
    use, intrinsic :: iso_fortran_env, only: real64, real128
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_positive_inf, &
        ieee_quiet_nan, ieee_is_finite, ieee_is_nan, ieee_is_negative
    implicit none
    real(real64), parameter :: samples(*) = [-4.0_real64, -0.125_real64, &
        0.125_real64, 3.0_real64, -1.0e155_real64, 1.0e155_real64, &
        -1.0e-155_real64, 1.0e-155_real64, huge(1.0_real64), &
        -huge(1.0_real64), tiny(1.0_real64), -tiny(1.0_real64)]
    real(real64) :: x, value, square, cube, tangent, adjoint, infinity, nan
    real(real64), parameter :: dx = -0.375_real64, seed = 1.25_real64
    real(real128) :: inverse
    integer :: i
    interface
        subroutine reciprocal_kernel(x, dx, seed, value, square, cube, tangent, adjoint)
            import real64
            real(real64), intent(in) :: x, dx, seed
            real(real64), intent(out) :: value, square, cube, tangent, adjoint
        end subroutine reciprocal_kernel
    end interface
    do i = 1, size(samples)
        x = samples(i)
        inverse = 1.0_real128/real(x,real128)
        call reciprocal_kernel(x, dx, seed, value, square, cube, tangent, adjoint)
        call close(value, inverse)
        call close(square, inverse**2)
        call close(cube, inverse**3)
        call close(tangent, -real(dx,real128)*inverse**2)
        call close(adjoint, -real(seed,real128)*inverse**2)
    end do
    ! Zero is outside the mathematical reciprocal/derivative domain. These
    ! checks retain IEEE classifications and signs, not a finite-value claim.
    call exceptional(0.0_real64, .false., .true.)
    call exceptional(-0.0_real64, .true., .true.)
    infinity = ieee_value(1.0_real64, ieee_positive_inf)
    call exceptional(infinity, .false., .false.)
    call exceptional(-infinity, .true., .false.)
    nan = ieee_value(1.0_real64, ieee_quiet_nan)
    call reciprocal_kernel(nan, dx, seed, value, square, cube, tangent, adjoint)
    if (.not. all(ieee_is_nan([value,square,cube,tangent,adjoint]))) error stop 'NaN propagation'
    print *, 'reciprocal primal/JVP/VJP: independent real128 and IEEE cases pass'
contains
    subroutine close(actual, expected)
        real(real64), intent(in) :: actual
        real(real128), intent(in) :: expected
        real(real128) :: tolerance
        if (abs(expected) > real(huge(actual),real128)) then
            if (ieee_is_finite(actual) .or. ieee_is_nan(actual)) error stop 'expected overflow'
            if (ieee_is_negative(actual) .neqv. (expected < 0)) error stop 'overflow sign'
            return
        end if
        if (.not. ieee_is_finite(actual)) error stop 'nonfinite finite-domain result'
        tolerance = 16.0_real128*real(epsilon(actual),real128) &
            *max(abs(expected),real(tiny(actual),real128))
        if (abs(real(actual,real128)-expected) > tolerance) error stop 'real128 mismatch'
    end subroutine close
    subroutine exceptional(input, negative, zero_input)
        real(real64), intent(in) :: input
        logical, intent(in) :: negative, zero_input
        call reciprocal_kernel(input, dx, seed, value, square, cube, tangent, adjoint)
        if (zero_input) then
            if (any(ieee_is_finite([value,square,cube,tangent,adjoint]))) error stop 'zero pole'
        else
            if (any([value,square,cube,tangent,adjoint] /= 0.0_real64)) error stop 'infinite input'
        end if
        if (ieee_is_negative(value) .neqv. negative) error stop 'reciprocal sign'
        if (ieee_is_negative(cube) .neqv. negative) error stop 'odd power sign'
        if (ieee_is_negative(square)) error stop 'even power sign'
        if (ieee_is_negative(tangent)) error stop 'tangent sign'
        if (.not. ieee_is_negative(adjoint)) error stop 'adjoint sign'
    end subroutine exceptional
end program test_reciprocal_kernel
