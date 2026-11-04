program test_horner_storage
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use horner_storage_kernels, only: horner_storage64, horner_storage32
    implicit none
    integer, parameter :: qp = selected_real_kind(30)
    real(real64), parameter :: h(3) = [0.0005_real64, 0.2_real64, 8.0_real64]
    real(real64), parameter :: sentinel = -7.25e300_real64
    real(real64) :: c(3, 0:5, 0:5, 0:5), x(3), out(14), dy(3, 3)
    real(real32) :: low(3, 0:5, 0:5, 0:5)
    real(qp) :: ref(12), mag(12), errors(12), bound(12)
    integer :: trial, i, j, k, q, variant
    logical :: rejected_axis, rejected_scale, rejected_narrow_output

    rejected_axis = .false.
    rejected_scale = .false.
    rejected_narrow_output = .false.
    do trial = 0, 12
        do k = 0, 5
            do j = 0, 5
                do i = 0, 5
                    do q = 1, 3
                        c(q, i, j, k) = (real(mod(17*i + 11*j + 7*k + q, &
                            29), real64) - 14.1_real64)/ &
                            (31*h(1)**i*h(2)**j*h(3)**k)
                    end do
                end do
            end do
        end do
        if (trial == 0) c = 0
        x = h*[real(trial, real64)/13, 0.71_real64, -0.23_real64]
        if (trial == 12) x = h
        low = real(c, real32)
        do variant = 1, 2
            out = sentinel
            if (variant == 1) then
                call horner_storage64(c, x(1), x(2), x(3), out(2:4), dy)
                call oracle(c, x, ref, mag)
            else
                call horner_storage32(low, x(1), x(2), x(3), out(2:4), dy)
                call oracle(real(low, real64), x, ref, mag)
            end if
            out(5:13) = reshape(dy, [9])
            if (out(1) /= sentinel .or. out(14) /= sentinel) error stop 'guards'
            if (.not. all(ieee_is_finite(out(2:13)))) error stop 'nonfinite output'
            errors = abs(real(out(2:13), qp) - ref)
            bound = 64*epsilon(1.0_real64)*max(mag, tiny(1.0_real64))
            if (any(errors > bound)) error stop 'storage oracle'
            if (trial > 0) then
                rejected_axis = rejected_axis .or. &
                    abs(real(dy(2, 1), qp) - ref(4)) > bound(4)
                rejected_scale = rejected_scale .or. &
                    abs(real(2*dy(1, 1), qp) - ref(4)) > bound(4)
                rejected_narrow_output = rejected_narrow_output .or. &
                    any(abs(real(real(out(2:13), real32), qp) - ref) > bound)
            end if
        end do
    end do
    if (.not. rejected_axis) error stop 'wrong-axis control survived'
    if (.not. rejected_scale) error stop 'derivative-scale control survived'
    if (.not. rejected_narrow_output) error stop 'narrow-output control survived'

contains

    subroutine oracle(coefficients, point, reference, magnitude)
        real(real64), intent(in) :: coefficients(3, 0:5, 0:5, 0:5), point(3)
        real(qp), intent(out) :: reference(12), magnitude(12)
        real(qp) :: term, power
        integer :: a, b, c, quantity, axis, exponent(3), d, slot

        reference = 0
        magnitude = 0
        do c = 0, 5
            do b = 0, 5
                do a = 0, 5
                    do quantity = 1, 3
                        do axis = 0, 3
                            exponent = [a, b, c]
                            term = real(coefficients(quantity, a, b, c), qp)
                            if (axis > 0) then
                                if (exponent(axis) == 0) cycle
                                term = term*exponent(axis)
                                exponent(axis) = exponent(axis) - 1
                            end if
                            do d = 1, 3
                                power = real(point(d), qp)**exponent(d)
                                term = term*power
                            end do
                            slot = quantity
                            if (axis > 0) slot = 3 + axis + 3*(quantity - 1)
                            reference(slot) = reference(slot) + term
                            magnitude(slot) = magnitude(slot) + abs(term)
                        end do
                    end do
                end do
            end do
        end do
    end subroutine
end program
