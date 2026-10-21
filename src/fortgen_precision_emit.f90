module fortgen_precision_emit
    !! One IR, four CPU variants. Half formats are storage, never fake kinds.
    use fortgen_kernel_ir, only: kernel_ir_t
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        PRECISION_REAL32, PRECISION_REAL64, TARGET_FORTRAN_CPU, TARGET_DEFAULT
    use fortgen_names, only: valid_fortran_name, lower_fortran_name
    use fortgen_buffer, only: buffer_t
    use fortgen_layout, only: put_wrapped
    use fortgen_string, only: str_t, str, chars
    implicit none
    private
    public :: emit_cpu_precision_variants
contains
    function emit_cpu_precision_variants(ir, spec, ok, message, compute) result(source)
        type(kernel_ir_t), intent(in) :: ir
        type(kernel_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer, intent(in), optional :: compute
        type(str_t) :: source, leaf
        type(buffer_t) :: wrapped
        type(kernel_emit_spec_t) :: variant
        character(4), parameter :: suffix(4) = ['fp64', 'fp32', 'fp16', 'bf16']
        character(:), allocatable :: text, prefix, name, kind_name, codec
        character(20) :: index_text
        integer :: i, j, arithmetic, start, finish

        source = str('')
        ok = .false.
        message = 'precision family requires CPU scalar arguments and outputs'
        if (spec%target /= TARGET_DEFAULT .and. spec%target /= TARGET_FORTRAN_CPU) return
        if (.not. allocated(spec%args)) return
        if (.not. allocated(spec%outputs)) return
        name = chars(spec%name)
        if (.not. valid_fortran_name(name//'_fp64')) return
        if (len_trim(name) > 58) return
        prefix = 'fgp_'
        do j = 1, size(spec%args)
            if (.not. valid_fortran_name(chars(spec%args(j)))) return
            if (reserved(chars(spec%args(j)))) return
        end do
        do j = 1, size(spec%outputs)
            if (.not. valid_fortran_name(chars(spec%outputs(j)))) return
            if (reserved(chars(spec%outputs(j)))) return
        end do
        ! Private identifiers must not shadow any caller-selected argument.
        do
            if (.not. any_prefix(spec, prefix)) exit
            prefix = prefix//'p'
        end do
        arithmetic = PRECISION_REAL32
        if (present(compute)) arithmetic = compute
        message = 'half-storage compute must be real32 or real64'
        if (arithmetic /= PRECISION_REAL32 .and. arithmetic /= PRECISION_REAL64) return
        kind_name = 'real32'
        if (arithmetic == PRECISION_REAL64) kind_name = 'real64'
        text = ''
        do i = 1, 4
            variant = spec
            variant%name = str(name//'_'//suffix(i))
            variant%precision = PRECISION_REAL64
            if (i == 2) variant%precision = PRECISION_REAL32
            if (i >= 3) then
                variant%name = str(prefix//'compute')
                variant%precision = arithmetic
            end if
            variant%pure_procedure = .true.
            leaf = emit_fortran_kernel_ir(ir, variant, ok, message)
            if (.not. ok) return
            if (i < 3) then
                text = text//chars(leaf)//new_line('a')
                cycle
            end if
            codec = trim(suffix(i))
            text = text//'! storage='//codec//' compute='//kind_name// &
                ' rounding=nearest-even'//new_line('a')
            text = text//'pure subroutine '//name//'_'//suffix(i)//'('
            do j = 1, size(spec%args)
                if (j > 1) text = text//', '
                text = text//chars(spec%args(j))
            end do
            do j = 1, size(spec%outputs)
                if (j > 1 .or. size(spec%args) > 0) text = text//', '
                text = text//chars(spec%outputs(j))
            end do
            text = text//')'//new_line('a')// &
                '    use iso_fortran_env, only: int16, real32, real64'//new_line('a')// &
                '    use fortgen_storage, only: pack_'//codec//', unpack_'// &
                codec//new_line('a')//'    implicit none'//new_line('a')
            do j = 1, size(spec%args)
                text = text//'    integer(int16), intent(in) :: '// &
                    chars(spec%args(j))//new_line('a')
            end do
            do j = 1, size(spec%outputs)
                write(index_text, '(i0)') j
                text = text//'    integer(int16), intent(out) :: '// &
                    chars(spec%outputs(j))//new_line('a')// &
                    '    real('//kind_name//') :: '//prefix//'y'// &
                    trim(index_text)//new_line('a')
            end do
            text = text//'    call '//prefix//'compute('
            do j = 1, size(spec%args)
                if (j > 1) text = text//', '
                text = text//'real(unpack_'//codec//'('// &
                    chars(spec%args(j))//'), '//kind_name//')'
            end do
            do j = 1, size(spec%outputs)
                if (j > 1 .or. size(spec%args) > 0) text = text//', '
                write(index_text, '(i0)') j
                text = text//prefix//'y'//trim(index_text)
            end do
            text = text//')'//new_line('a')
            do j = 1, size(spec%outputs)
                write(index_text, '(i0)') j
                text = text//'    '//chars(spec%outputs(j))//' = pack_'// &
                    codec//'(real('//prefix//'y'//trim(index_text)// &
                    ', real64))'//new_line('a')
            end do
            text = text//'contains'//new_line('a')//chars(leaf)// &
                'end subroutine '//name//'_'//suffix(i)//new_line('a')
        end do
        start = 1
        do while (start <= len(text))
            finish = index(text(start:), new_line('a'))
            if (finish == 0) exit
            finish = start + finish - 2
            if (finish >= start) then
                if (text(start:start) == '!') then
                    call wrapped%line(text(start:finish))
                else
                    call put_wrapped(wrapped, '', text(start:finish))
                end if
            else
                call wrapped%line('')
            end if
            start = finish + 2
        end do
        source = str(wrapped%str())
        ok = .true.
        message = ''
    end function

    logical function reserved(name) result(found)
        character(*), intent(in) :: name
        character(:), allocatable :: lower
        lower = lower_fortran_name(name)
        found = lower == 'int16' .or. lower == 'real32' .or. lower == 'real64'
        found = found .or. index(lower, 'pack_') == 1
        found = found .or. index(lower, 'unpack_') == 1
    end function

    logical function any_prefix(spec, prefix) result(found)
        type(kernel_emit_spec_t), intent(in) :: spec
        character(*), intent(in) :: prefix
        integer :: i
        found = .false.
        do i = 1, size(spec%args)
            if (index(lower_fortran_name(chars(spec%args(i))), prefix) == 1) found = .true.
        end do
        do i = 1, size(spec%outputs)
            if (index(lower_fortran_name(chars(spec%outputs(i))), prefix) == 1) found = .true.
        end do
    end function
end module fortgen_precision_emit
