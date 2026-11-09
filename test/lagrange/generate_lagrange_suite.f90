program generate_lagrange_suite
    ! Usage: generate_lagrange_suite KERNELS CASES
    ! Writes every case kernel to KERNELS and the case table and dispatch,
    ! which the test reads, to CASES.
    use fortgen_lagrange_emit, only: lagrange_kernel_spec_t, emit_lagrange_kernel
    use fortgen_kernel_target, only: TARGET_FORTRAN_CPU, TARGET_FORTRAN_OPENACC
    use fortgen_string, only: str, chars
    implicit none
    integer, parameter :: NCASE = 10
    ! d, n1, n2, n3, nq (0 = runtime), gradient, openacc spelling
    integer, parameter :: table(7, NCASE) = reshape([ &
        1, 2, 0, 0, 1, 1, 0, &
        1, 6, 0, 0, 0, 1, 0, &
        1, 10, 0, 0, 2, 0, 0, &
        2, 4, 4, 0, 1, 1, 0, &
        2, 3, 5, 0, 0, 1, 1, &
        3, 4, 4, 4, 1, 1, 0, &
        3, 6, 6, 6, 1, 1, 0, &
        3, 2, 3, 4, 0, 1, 0, &
        3, 6, 6, 6, 3, 0, 0, &
        3, 4, 4, 4, 0, 1, 1], [7, NCASE])
    character(2048) :: kernels_path, cases_path
    type(lagrange_kernel_spec_t) :: spec
    character(:), allocatable :: message
    logical :: ok
    integer :: kernels, cases, i, j

    call get_command_argument(1, kernels_path)
    call get_command_argument(2, cases_path)
    call refusals()
    open (newunit=kernels, file=trim(kernels_path), status='replace', action='write')
    write (kernels, '(a)') 'module lagrange_suite_kernels'
    write (kernels, '(a)') 'implicit none'
    write (kernels, '(a)') 'contains'
    do i = 1, NCASE
        spec = case_spec(i)
        block
            character(:), allocatable :: src
            src = chars(emit_lagrange_kernel(spec, ok, message))
            if (.not. ok) error stop message
            write (kernels, '(a)') src
        end block
    end do
    write (kernels, '(a)') 'end module lagrange_suite_kernels'
    close (kernels)

    open (newunit=cases, file=trim(cases_path), status='replace', action='write')
    write (cases, '(a,i0)') 'integer, parameter :: lagrange_ncase = ', NCASE
    write (cases, '(a)') 'integer, parameter :: lagrange_table(7, lagrange_ncase) = reshape([ &'
    do i = 1, NCASE
        write (cases, '(a,6(i0,a),i0,a)') '    ', (table(j, i), ', ', j = 1, 6), &
            table(7, i), trim(merge(', & ', '], &', i < NCASE))
    end do
    write (cases, '(a)') '    [7, lagrange_ncase])'
    write (cases, '(a)') 'contains'
    write (cases, '(a)') 'subroutine lagrange_run_case(id, nq, f, t, h, y, dy)'
    write (cases, '(a)') 'use lagrange_suite_kernels'
    write (cases, '(a)') 'integer, intent(in) :: id, nq'
    write (cases, '(a)') 'real(real64), intent(in) :: f(*), t(3), h(3)'
    write (cases, '(a)') 'real(real64), intent(out) :: y(*), dy(*)'
    write (cases, '(a)') 'select case (id)'
    do i = 1, NCASE
        write (cases, '(a,i0,a)') 'case (', i, ')'
        write (cases, '(a,i0,a)') '    call run', i, '(f, y, dy)'
    end do
    write (cases, '(a)') 'end select'
    write (cases, '(a)') 'contains'
    do i = 1, NCASE
        call write_runner(i)
    end do
    write (cases, '(a)') 'end subroutine lagrange_run_case'
    close (cases)
contains
    function case_spec(i) result(s)
        integer, intent(in) :: i
        type(lagrange_kernel_spec_t) :: s
        character(8) :: nm

        write (nm, '(a,i0)') 'lag', i
        s%name = str(trim(nm))
        s%dimension = table(1, i)
        s%points = table(2:4, i)
        s%nq = table(5, i)
        s%gradient = table(6, i) == 1
        s%target = merge(TARGET_FORTRAN_OPENACC, TARGET_FORTRAN_CPU, table(7, i) == 1)
    end function

    ! Wrapper that reshapes the flat test arrays to the kernel's interface.
    subroutine write_runner(i)
        integer, intent(in) :: i
        integer :: d, k
        character(256) :: dims, call_args
        character(16) :: nqs

        d = table(1, i)
        if (table(5, i) == 0) then
            nqs = 'nq'
        else
            write (nqs, '(i0)') table(5, i)
        end if
        write (dims, '(a)') trim(nqs)
        do k = 1, d
            write (dims, '(a,a,i0)') trim(dims), ', ', table(1 + k, i)
        end do
        call_args = ''
        if (table(5, i) == 0) call_args = 'nq, '
        call_args = trim(call_args)//' f'
        do k = 1, d
            write (call_args, '(a,a,i0,a)') trim(call_args), ', t(', k, ')'
        end do
        do k = 1, d
            write (call_args, '(a,a,i0,a)') trim(call_args), ', h(', k, ')'
        end do
        call_args = trim(call_args)//', y'
        if (table(6, i) == 1) call_args = trim(call_args)//', dy'
        write (cases, '(a,i0,a)') 'subroutine run', i, '(f, y, dy)'
        write (cases, '(a)') 'real(real64), intent(in) :: f('//trim(dims)//')'
        write (cases, '(a)') 'real(real64), intent(out) :: y('//trim(nqs)//')'
        write (cases, '(a,i0,a)') 'real(real64), intent(out) :: dy(', d, ', '//trim(nqs)//')'
        write (cases, '(a,i0,a)') 'call lag', i, '('//trim(call_args)//')'
        write (cases, '(a,i0)') 'end subroutine run', i
    end subroutine

    subroutine refusals()
        type(lagrange_kernel_spec_t) :: s
        character(:), allocatable :: m
        logical :: ok
        character(:), allocatable :: src

        s%name = str('bad')
        s%dimension = 4
        src = chars(emit_lagrange_kernel(s, ok, m))
        if (ok .or. len(src) /= 0) error stop 'dimension 4 accepted'
        s%dimension = 2
        s%points = [1, 4, 0]
        src = chars(emit_lagrange_kernel(s, ok, m))
        if (ok) error stop 'one-point stencil accepted'
        s%points = [4, 4, 0]
        s%name = str('1bad')
        src = chars(emit_lagrange_kernel(s, ok, m))
        if (ok) error stop 'invalid name accepted'
    end subroutine
end program generate_lagrange_suite
