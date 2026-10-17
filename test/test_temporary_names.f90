program test_temporary_names
    use, intrinsic :: iso_fortran_env, only: real64, real128
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    real(real64) :: x, y, small, large
    real(real128) :: sum, reference, tolerance
    integer :: i
    interface
        subroutine temporary_name_kernel(x, y, value)
            import real64
            real(real64), intent(in) :: x, y
            real(real64), intent(out) :: value
        end subroutine
        subroutine temporary_name_kernel_large(x, y, value)
            import real64
            real(real64), intent(in) :: x, y
            real(real64), intent(out) :: value
        end subroutine
    end interface

    tolerance = 32.0_real128*real(epsilon(x), real128)
    do i = -32, 32
        x = real(i, real64)/8.0_real64
        y = real(17 - i, real64)/16.0_real64
        call temporary_name_kernel(x, y, small)
        call temporary_name_kernel_large(x, y, large)
        if (.not. all(ieee_is_finite([small, large]))) error stop "non-finite output"
        sum = real(x, real128) + real(y, real128)
        if (abs(real(small, real128) - sum) > &
            tolerance*max(1.0_real128, abs(sum))) error stop "sum mismatch"
        ! Factored independent formula for the expanded emitted polynomial.
        reference = sum*(sum + 1.0_real128)
        if (abs(real(large, real128) - reference) > &
            tolerance*max(1.0_real128, abs(reference))) error stop "polynomial mismatch"
    end do
    print *, "temporary-name boundary:65 independent real128 samples pass"
end program test_temporary_names
