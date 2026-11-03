module horner_suite_cases
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    include 'horner_cases.inc'
end module horner_suite_cases

program test_horner
    ! Compares every emitted Horner kernel with a dense monomial oracle in
    ! extended precision: d^a p = sum_k c_k prod_j k_j!/(k_j-a_j)! x_j**(k_j-a_j).
    use, intrinsic :: iso_fortran_env, only: real64
    use horner_suite_cases, only: horner_ncase, horner_case_info, horner_run_case
    implicit none
    integer, parameter :: qp = selected_real_kind(30)
    integer, parameter :: ntrial = 25, nout_max = 4000
    real(real64), parameter :: sentinel = -7.25e300_real64
    integer :: id, trial, d, n(3), nq_fixed, nq, norders, orders(3, 200)
    integer :: ncoeff, iq, j, slot, m, nm, offset, k1, k2, k3, total, idx
    real(real64), allocatable :: c(:)
    real(real64) :: x(3), out(nout_max), worst, err, scale
    real(qp) :: ref, term, mag
    integer :: failures

    failures = 0
    worst = 0
    call random_seed(put=[(17*j + 3, j=1, 64)])
    do id = 1, horner_ncase
        call horner_case_info(id, d, n, nq_fixed, norders, orders)
        nq = nq_fixed
        if (nq == 0) nq = 3
        ncoeff = nq*product(n(1:d) + 1)
        allocate (c(ncoeff))
        do trial = 1, ntrial
            call random_number(c)
            c = 2*c - 1
            call random_number(x)
            x = 1.5_real64*x - 0.25_real64
            out = sentinel
            call horner_run_case(id, nq, c, x, out)
            total = norders*nq
            if (any(out(total + 1:) /= sentinel)) then
                print '(a,i0)', 'write beyond outputs in case ', id
                failures = failures + 1
            end if
            offset = 0
            do m = 0, 3
                nm = count(sum(orders(:, 1:norders), 1) == m)
                slot = 0
                do j = 1, norders
                    if (sum(orders(:, j)) /= m) cycle
                    slot = slot + 1
                    do iq = 1, nq
                        ref = 0
                        mag = 0
                        do k3 = 0, merge(n(3), 0, d >= 3)
                            do k2 = 0, merge(n(2), 0, d >= 2)
                                do k1 = 0, n(1)
                                    idx = iq + nq*(k1 + (n(1) + 1)*(k2 + &
                                        (n(2) + 1)*k3))
                                    term = real(c(idx), qp)* &
                                        monomial(k1, orders(1, j), x(1))* &
                                        monomial(k2, orders(2, j), x(2))* &
                                        monomial(k3, orders(3, j), x(3))
                                    ref = ref + term
                                    mag = mag + abs(term)
                                end do
                            end do
                        end do
                        idx = offset*nq + slot + nm*(iq - 1)
                        err = real(abs(real(out(idx), qp) - ref), real64)
                        scale = max(real(mag, real64), tiny(1.0_real64))
                        worst = max(worst, err/scale)
                        if (err > 64*epsilon(1.0_real64)*scale) then
                            print '(a,i0,a,3i2,a,i0,2es24.15)', 'case ', id, &
                                ' derivative ', orders(:, j), ' quantity ', iq, &
                                out(idx), real(ref, real64)
                            failures = failures + 1
                        end if
                    end do
                end do
                offset = offset + nm
            end do
        end do
        deallocate (c)
    end do
    print '(a,es10.3)', 'worst error relative to term magnitude: ', worst
    if (failures > 0) error stop 'Horner kernels disagree with the dense oracle'

contains

    !> d^a/dx^a of x**k, exactly in extended precision.
    function monomial(k, a, x) result(v)
        integer, intent(in) :: k, a
        real(real64), intent(in) :: x
        real(qp) :: v
        integer :: i

        if (a > k) then
            v = 0
            return
        end if
        v = 1
        do i = k - a + 1, k
            v = v*i
        end do
        v = v*real(x, qp)**(k - a)
    end function
end program test_horner
