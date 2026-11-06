program generate_horner_suite
    ! Usage: generate_horner_suite fortran|cuda|metal KERNELS CASES
    ! Writes every case kernel to KERNELS and, to CASES, the derivative
    ! multi-indices in emitted order for the independent oracle. The case
    ! dispatch goes to CASES, except for Metal, whose dispatch is device code
    ! and goes to KERNELS.
    use fortgen_horner_emit, only: horner_kernel_spec_t, emit_horner_kernel, &
        horner_derivative_count
    use fortgen_derivative_set, only: partials, radial_mixed, multi_indices
    use fortgen_kernel_ir, only: TYPE_REAL32, TYPE_REAL64
    use fortgen_kernel_target, only: TARGET_FORTRAN_CPU, TARGET_FORTRAN_OPENACC, &
        TARGET_CUDA, TARGET_METAL
    use fortgen_string, only: str, str_t, chars
    implicit none
    integer, parameter :: NCASE = 16
    ! d, n1, n2, n3, nq (0 = runtime), max order, rmix
    integer, parameter :: table(7, NCASE) = reshape([ &
        1, 5, 0, 0, 0, 3, 0, &
        1, 5, 0, 0, 1, 2, 0, &
        1, 0, 0, 0, 2, 2, 0, &
        1, 1, 0, 0, 1, 3, 0, &
        1, 6, 0, 0, 3, 1, 0, &
        2, 3, 4, 0, 0, 2, 0, &
        2, 5, 1, 0, 2, 3, 0, &
        3, 5, 5, 5, 1, 0, 0, &
        3, 5, 5, 5, 1, 1, 0, &
        3, 5, 5, 5, 1, 2, 0, &
        3, 5, 5, 5, 3, 2, 0, &
        3, 5, 5, 5, 0, 2, 0, &
        3, 5, 5, 5, 1, 2, 1, &
        3, 2, 3, 4, 2, 3, 0, &
        3, 3, 3, 3, 0, 1, 0, &
        3, 5, 5, 5, 0, 0, 0], [7, NCASE])
    character(2048) :: mode, kernels_path, cases_path
    integer :: kernels, cases, i
    logical :: cuda, metal

    call get_command_argument(1, mode)
    call get_command_argument(2, kernels_path)
    call get_command_argument(3, cases_path)
    metal = trim(mode) == 'metal'
    cuda = trim(mode) == 'cuda' .or. metal
    if (.not. cuda .and. trim(mode) /= 'fortran') error stop 'unknown mode'
    call refusals()
    open (newunit=kernels, file=trim(kernels_path), status='replace', action='write')
    open (newunit=cases, file=trim(cases_path), status='replace', action='write')
    if (metal) write (kernels, '(a)') '#include <metal_stdlib>'
    if (.not. cuda) then
        write (kernels, '(a)') 'module horner_suite_kernels'
        write (kernels, '(a)') 'implicit none'
        write (kernels, '(a)') 'contains'
    end if
    call write_info_header(cases)
    do i = 1, NCASE
        call write_case(i)
    end do
    if (metal) then
        call write_dispatch(kernels)
    else
        call write_dispatch(cases)
    end if
    if (.not. cuda) write (kernels, '(a)') 'end module horner_suite_kernels'
    close (kernels)
    close (cases)

contains

    function num(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(16) :: tmp
        write (tmp, '(i0)') i
        s = trim(tmp)
    end function

    function spec_of(i) result(spec)
        integer, intent(in) :: i
        type(horner_kernel_spec_t) :: spec

        spec%name = str('horner_case_'//num(i))
        spec%degree = table(2:4, i)
        spec%nq = table(5, i)
        if (table(7, i) == 1) then
            spec%derivatives = radial_mixed(table(1, i))
        else
            spec%derivatives = partials(table(1, i), table(6, i))
        end if
        if (metal) then
            spec%target = TARGET_METAL
            spec%coefficient_type = TYPE_REAL32
            spec%compute_type = TYPE_REAL32
        else if (cuda) then
            spec%target = TARGET_CUDA
        else if (mod(i, 2) == 0) then
            spec%target = TARGET_FORTRAN_OPENACC
        else
            spec%target = TARGET_FORTRAN_CPU
        end if
    end function

    !> Requested multi-indices in emitted order: total order, then descending
    !> lexicographic. Written independently of the emitter's own bookkeeping.
    subroutine requested(i, list, n)
        integer, intent(in) :: i
        integer, intent(out) :: list(3, 200), n
        integer :: m, a1, a2, a3, d, k, r(3, 7)
        logical :: take

        d = table(1, i)
        k = table(6, i)
        r = reshape([0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, &
            2, 0, 0, 1, 1, 0, 1, 0, 1], [3, 7])
        n = 0
        do m = 0, 3*9
            do a1 = 9, 0, -1
                do a2 = merge(9, 0, d >= 2), 0, -1
                    do a3 = merge(9, 0, d >= 3), 0, -1
                        if (a1 + a2 + a3 /= m) cycle
                        if (table(7, i) == 1) then
                            take = any(r(1, :) == a1 .and. r(2, :) == a2 .and. &
                                r(3, :) == a3)
                        else
                            take = m <= k
                        end if
                        if (.not. take) cycle
                        n = n + 1
                        list(:, n) = [a1, a2, a3]
                    end do
                end do
            end do
        end do
    end subroutine

    subroutine write_case(i)
        integer, intent(in) :: i
        type(horner_kernel_spec_t) :: spec
        type(str_t) :: source
        character(:), allocatable :: message
        logical :: ok
        integer :: list(3, 200), n, m

        spec = spec_of(i)
        source = emit_horner_kernel(spec, ok, message)
        if (.not. ok) then
            write (*, '(a)') message
            error stop 'valid case refused'
        end if
        call requested(i, list, n)
        do m = 0, 3
            if (horner_derivative_count(spec, m) /= count(sum(list(:, 1:n), 1) == m)) &
                error stop 'derivative count disagrees'
        end do
        write (kernels, '(a)') chars(source)
    end subroutine

    subroutine write_info_header(u)
        integer, intent(in) :: u
        integer :: i, j, list(3, 200), n

        if (cuda) then
            write (u, '(a)') 'static const int horner_ncase = '//num(NCASE)//';'
            write (u, '(a)') 'static const int horner_table[] = {'
            do i = 1, NCASE
                write (u, '(a)') '    '//num(table(1, i))//', '//num(table(2, i))// &
                    ', '//num(table(3, i))//', '//num(table(4, i))//', '// &
                    num(table(5, i))//','
            end do
            write (u, '(a)') '};'
            write (u, '(a)') 'static const int horner_norders[] = {'
            do i = 1, NCASE
                call requested(i, list, n)
                write (u, '(a)') '    '//num(n)//','
            end do
            write (u, '(a)') '};'
            write (u, '(a)') 'static const int horner_orders[] = {'
            do i = 1, NCASE
                call requested(i, list, n)
                do j = 1, n
                    write (u, '(a)') '    '//num(list(1, j))//', '//num(list(2, j))// &
                        ', '//num(list(3, j))//','
                end do
            end do
            write (u, '(a)') '};'
        else
            write (u, '(a)') 'integer, parameter :: horner_ncase = '//num(NCASE)
            write (u, '(a)') 'contains'
            write (u, '(a)') 'subroutine horner_case_info(id, d, n, nq, norders, orders)'
            write (u, '(a)') 'integer, intent(in) :: id'
            write (u, '(a)') 'integer, intent(out) :: d, n(3), nq, norders'
            write (u, '(a)') 'integer, intent(out) :: orders(3, 200)'
            write (u, '(a)') 'orders = 0'
            write (u, '(a)') 'select case (id)'
            do i = 1, NCASE
                call requested(i, list, n)
                write (u, '(a)') 'case ('//num(i)//')'
                write (u, '(a)') 'd = '//num(table(1, i))//'; n = ['// &
                    num(table(2, i))//', '//num(table(3, i))//', '// &
                    num(table(4, i))//']; nq = '//num(table(5, i))// &
                    '; norders = '//num(n)
                do j = 1, n
                    write (u, '(a)') 'orders(:, '//num(j)//') = ['// &
                        num(list(1, j))//', '//num(list(2, j))//', '// &
                        num(list(3, j))//']'
                end do
            end do
            write (u, '(a)') 'case default'
            write (u, '(a)') 'error stop "unknown case"'
            write (u, '(a)') 'end select'
            write (u, '(a)') 'end subroutine'
        end if
    end subroutine

    !> Output m of case i starts at out(offset) and holds count_m*nq values.
    subroutine write_dispatch(u)
        integer, intent(in) :: u
        integer :: i, m, list(3, 200), n, d, nm, offset
        character(:), allocatable :: call_args, nq

        if (metal) then
            ! Leaves write thread-space outputs; the launch copies them out.
            write (u, '(a)') 'kernel void horner_run_case(constant int& id, '// &
                'constant int& nq, device const float* c, device const float* x, '// &
                'device float* result)'
            write (u, '(a)') '{'
            write (u, '(a)') '    thread float out[256];'
            write (u, '(a)') '    for (int k = 0; k < 256; ++k) out[k] = 0.0f;'
            write (u, '(a)') '    switch (id) {'
        else if (cuda) then
            write (u, '(a)') '__global__ void horner_run_case(int id, int nq, '// &
                'const double* c, const double* x, double* out)'
            write (u, '(a)') '{'
            write (u, '(a)') '    switch (id) {'
        else
            write (u, '(a)') 'subroutine horner_run_case(id, nq, c, x, out)'
            write (u, '(a)') 'use horner_suite_kernels'
            write (u, '(a)') 'integer, intent(in) :: id, nq'
            write (u, '(a)') 'real(real64), intent(in) :: c(*), x(3)'
            write (u, '(a)') 'real(real64), intent(out) :: out(*)'
            write (u, '(a)') 'select case (id)'
        end if
        do i = 1, NCASE
            call requested(i, list, n)
            d = table(1, i)
            nq = 'nq'
            call_args = ''
            if (table(5, i) == 0) call_args = 'nq, '
            if (cuda) then
                call_args = call_args//'c'
                call_args = call_args//', x[0]'
                if (d >= 2) call_args = call_args//', x[1]'
                if (d >= 3) call_args = call_args//', x[2]'
            else
                call_args = call_args//'c'
                call_args = call_args//', x(1)'
                if (d >= 2) call_args = call_args//', x(2)'
                if (d >= 3) call_args = call_args//', x(3)'
            end if
            offset = 0
            do m = 0, 3
                nm = count(sum(list(:, 1:n), 1) == m)
                if (nm == 0) cycle
                if (cuda) then
                    call_args = call_args//', out + '//num(offset)//'*nq'
                else
                    call_args = call_args//', out(1 + '//num(offset)//'*nq)'
                end if
                offset = offset + nm
            end do
            if (cuda) then
                write (u, '(a)') '    case '//num(i)//': horner_case_'//num(i)// &
                    '('//call_args//'); break;'
            else
                write (u, '(a)') 'case ('//num(i)//')'
                write (u, '(a)') 'call horner_case_'//num(i)//'('//call_args//')'
            end if
        end do
        if (metal) then
            write (u, '(a)') '    default: break;'
            write (u, '(a)') '    }'
            write (u, '(a)') '    for (int k = 0; k < 256; ++k) result[k] = out[k];'
            write (u, '(a)') '}'
        else if (cuda) then
            write (u, '(a)') '    default: break;'
            write (u, '(a)') '    }'
            write (u, '(a)') '}'
        else
            write (u, '(a)') 'case default'
            write (u, '(a)') 'error stop "unknown case"'
            write (u, '(a)') 'end select'
            write (u, '(a)') 'end subroutine'
        end if
    end subroutine

    subroutine refusals()
        type(horner_kernel_spec_t) :: bad
        type(str_t) :: s
        character(:), allocatable :: message
        logical :: ok

        bad = spec_of(10)
        bad%derivatives = partials(4, 2)
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'dimension 4 accepted'
        bad = spec_of(10)
        bad%degree(2) = -1
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'negative degree accepted'
        bad = spec_of(10)
        bad%nq = -1
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'negative nq accepted'
        bad = spec_of(10)
        bad%target = 99
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'unknown target accepted'
        bad = spec_of(10)
        bad%name = str('1bad')
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'invalid name accepted'
        bad = spec_of(10)
        bad%derivatives = multi_indices(reshape([0, 0, 0, 0, 0, 0], [3, 2]))
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'duplicate order accepted'
        bad = spec_of(10)
        deallocate (bad%derivatives%orders)
        allocate (bad%derivatives%orders(2, 1))
        bad%derivatives%orders = 0
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'order rank mismatch accepted'
        bad = spec_of(10)
        bad%target = TARGET_METAL
        bad%coefficient_type = TYPE_REAL64
        bad%compute_type = TYPE_REAL64
        s = emit_horner_kernel(bad, ok, message)
        if (ok .or. len(chars(s)) /= 0) error stop 'binary64 Metal accepted'
    end subroutine
end program generate_horner_suite
