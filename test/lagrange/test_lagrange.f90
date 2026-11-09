module lagrange_suite_cases
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    include 'lagrange_cases.inc'
end module lagrange_suite_cases

program test_lagrange
    ! Compares every emitted Lagrange point kernel with a dense oracle in
    ! extended precision, sum_i f_i prod_j L_{i_j}(t_j), and its gradient
    ! h_j d/dt_j, at random points inside and slightly outside the stencil.
    ! The error bound is 64 ulp of the summed term magnitudes.
    use, intrinsic :: iso_fortran_env, only: real64
    use lagrange_suite_cases, only: lagrange_ncase, lagrange_table, lagrange_run_case
    implicit none
    integer, parameter :: qp = selected_real_kind(30), ntrial = 25
    real(real64), parameter :: sentinel = -7.25e300_real64
    integer :: id, trial, d, n(3), nq, iq, i1, i2, i3, j, nf, failures, k
    logical :: grad
    real(real64), allocatable :: f(:)
    real(real64) :: t(3), h(3), y(8), dy(24), err, worst
    real(qp) :: l(10, 3), dl(10, 3), ref(0:3), mag(0:3), term(0:3)

    failures = 0
    worst = 0
    call random_seed(put=[(31*j + 7, j=1, 64)])
    do id = 1, lagrange_ncase
        d = lagrange_table(1, id)
        n = max(lagrange_table(2:4, id), 1)
        nq = lagrange_table(5, id)
        if (nq == 0) nq = 3
        grad = lagrange_table(6, id) == 1
        nf = nq*product(n(1:d))
        allocate (f(nf))
        do trial = 1, ntrial
            call random_number(f)
            f = 2*f - 1
            call random_number(t)
            call random_number(h)
            h = 0.5_real64 + 4*h
            do j = 1, 3
                t(j) = (n(j) - 1)*(1.2_real64*t(j) - 0.1_real64)
            end do
            y = sentinel
            dy = sentinel
            call lagrange_run_case(id, nq, f, t, h, y, dy)
            if (any(y(nq + 1:) /= sentinel)) failures = failures + 1
            if (.not. grad .and. any(dy /= sentinel)) failures = failures + 1
            if (grad .and. any(dy(d*nq + 1:) /= sentinel)) failures = failures + 1
            do j = 1, d
                call weights(n(j), real(t(j), qp), l(:, j), dl(:, j))
            end do
            do iq = 1, nq
                ref = 0
                mag = 0
                do i3 = 1, merge(n(3), 1, d >= 3)
                    do i2 = 1, merge(n(2), 1, d >= 2)
                        do i1 = 1, n(1)
                            k = iq + nq*((i1 - 1) + n(1)*((i2 - 1) + n(2)*(i3 - 1)))
                            term = real(f(k), qp)*basis(i1, i2, i3)
                            ref = ref + term
                            mag = mag + abs(term)
                        end do
                    end do
                end do
                err = real(abs(y(iq) - ref(0))/max(mag(0), tiny(1.0_qp)), real64)
                worst = max(worst, err/epsilon(1.0_real64))
                if (err > 64*epsilon(1.0_real64)) then
                    print '(a,i0,a,es10.3)', 'value case ', id, ' rel ', err
                    failures = failures + 1
                end if
                if (.not. grad) cycle
                do j = 1, d
                    err = real(abs(dy(j + d*(iq - 1)) - h(j)*ref(j)) &
                               /max(h(j)*mag(j), tiny(1.0_qp)), real64)
                    worst = max(worst, err/epsilon(1.0_real64))
                    if (err > 64*epsilon(1.0_real64)) then
                        print '(a,i0,a,i0,a,es10.3)', 'gradient case ', id, ' dir ', j, &
                            ' rel ', err
                        failures = failures + 1
                    end if
                end do
            end do
        end do
        deallocate (f)
    end do
    print '(a,i0,a,f6.1,a)', 'lagrange: ', lagrange_ncase, ' cases, worst ', worst, ' ulp'
    if (failures > 0) error stop 'lagrange kernel mismatch'
contains
    function basis(i1, i2, i3) result(b)
        integer, intent(in) :: i1, i2, i3
        real(qp) :: b(0:3), v(3), dv(3)
        integer :: idx(3), jj

        idx = [i1, i2, i3]
        v = 1
        dv = 0
        do jj = 1, d
            v(jj) = l(idx(jj), jj)
            dv(jj) = dl(idx(jj), jj)
        end do
        b(0) = product(v)
        b(1) = dv(1)*v(2)*v(3)
        b(2) = v(1)*dv(2)*v(3)
        b(3) = v(1)*v(2)*dv(3)
    end function

    subroutine weights(m, x, w, dw)
        integer, intent(in) :: m
        real(qp), intent(in) :: x
        real(qp), intent(out) :: w(10), dw(10)
        integer :: i, kk, p
        real(qp) :: term

        w = 0
        dw = 0
        do i = 1, m
            w(i) = 1
            do kk = 1, m
                if (kk /= i) w(i) = w(i)*(x - (kk - 1))/(i - kk)
            end do
            do kk = 1, m
                if (kk == i) cycle
                term = 1.0_qp/(i - kk)
                do p = 1, m
                    if (p /= i .and. p /= kk) term = term*(x - (p - 1))/(i - p)
                end do
                dw(i) = dw(i) + term
            end do
        end do
    end subroutine
end program test_lagrange
