program test_linewrap_kernel
    use, intrinsic :: iso_fortran_env, only: real64, real128
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortgen_buffer, only: buffer_t
    use fortgen_layout, only: put_wrapped
    implicit none
    integer :: i
    real(real64) :: x, a, b, c, d, h, fourth, scaled
    real(real128) :: xr, reference, reference_scaled, tolerance
    interface
        subroutine hermite_kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk( &
            x, a, b, c, d, h, fourth, scaled)
            import real64
            real(real64), intent(in) :: x, a, b, c, d
            real(real64), intent(out) :: h, fourth, scaled
        end subroutine
    end interface

    call check_quoted_literals()
    tolerance = 128.0_real128*real(epsilon(x), real128)
    do i = -16, 48
        x = real(i, real64)/32.0_real64
        a = real(2*i - 7, real64)/16.0_real64
        b = real(11 - i, real64)/8.0_real64
        c = real(i + 3, real64)/32.0_real64
        d = real(5 - 3*i, real64)/16.0_real64
        call hermite_kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk( &
            x, a, b, c, d, h, fourth, scaled)
        xr = real(x, real128)
        ! Independent factored Hermite shape functions, rather than the
        ! expanded polynomial in the serialized computation graph.
        reference = (1.0_real128 - xr)**2*(1.0_real128 + 2*xr)*a &
            + xr*xr*(3.0_real128 - 2*xr)*b &
            + xr*(1.0_real128 - xr)**2*c &
            - xr*xr*(1.0_real128 - xr)*d
        if (.not. ieee_is_finite(h)) error stop "non-finite Hermite value"
        if (abs(real(h, real128) - reference) > &
            tolerance*max(1.0_real128, abs(reference))) error stop "Hermite mismatch"
        reference = xr*xr*xr*xr
        if (abs(real(fourth, real128) - reference) > &
            tolerance*max(1.0_real128, abs(reference))) error stop "power mismatch"
        reference_scaled = -1.23456789012345_real128*xr
        if (abs(real(scaled, real128)*1.0e120_real128 - reference_scaled) > &
            tolerance*max(1.0_real128, abs(reference_scaled))) &
            error stop "scientific literal mismatch"
    end do
    print *, "linewrap kernel: 65 independent real128 Hermite samples passed"
contains
    subroutine check_quoted_literals()
        type(buffer_t) :: text
        character(:), allocatable :: literal, wrapped
        integer :: k, start, stop

        do k = 1, 2
            if (k == 1) then
                literal = '"one, two (""three"")"'
            else
                literal = "'one, two (''three'')'"
            end if
            call text%reset()
            call put_wrapped(text, "", "call long_consumer_name("//literal// &
                ", argument_number_one, argument_number_two)", limit=40)
            wrapped = text%str()
            if (index(wrapped, literal) == 0) error stop "split quoted literal"
            start = 1
            do while (start <= len(wrapped))
                stop = index(wrapped(start:), new_line("a"))
                if (stop == 0) stop = len(wrapped) - start + 2
                if (stop - 1 > 40) error stop "quoted fixture exceeds line limit"
                start = start + stop
            end do
        end do
    end subroutine check_quoted_literals
end program test_linewrap_kernel
