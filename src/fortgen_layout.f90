module fortgen_layout
    !! Line layout for generated Fortran.
    !!
    !! Generated statements are long. A chain rule over a handful of operations,
    !! or an expanded polynomial, passes 132 columns routinely - and an
    !! over-long line is not valid Fortran, so continuation is a correctness
    !! requirement rather than a matter of taste.
    !!
    !! The one rule that is easy to get wrong: a break must never land inside a
    !! token. Splitting `a ** b` between its asterisks, or a numeric literal
    !! anywhere, produces text that either fails to compile or - worse - parses
    !! as something else.
    use fortgen_buffer, only: buffer_t
    implicit none
    private

    public :: put_wrapped, indent_of, DEFAULT_LINE_LIMIT

    !! The standard allows 132 columns. Stopping well short leaves room for
    !! indentation and the trailing ampersand, and keeps generated code
    !! readable in a side-by-side review.
    integer, parameter :: DEFAULT_LINE_LIMIT = 88

contains

    subroutine put_wrapped(b, indent, line, limit)
        !! Append one statement, continuing it before the column limit.
        type(buffer_t), intent(inout) :: b
        character(len=*), intent(in) :: indent, line
        !! Column limit, default `DEFAULT_LINE_LIMIT`.
        integer, intent(in), optional :: limit
        integer :: start, remaining, room, cut, cols

        cols = DEFAULT_LINE_LIMIT
        if (present(limit)) cols = limit

        room = cols - len(indent)
        if (room < 16) room = 16
        if (len(line) <= room) then
            call b%line(indent//line)
            return
        end if

        start = 1
        do
            ! Continuation lines carry four more columns of indentation.
            if (start > 1) room = max(16, cols - len(indent) - 4)
            remaining = len(line) - start + 1
            if (remaining <= room) then
                call b%line(indent//"    "//line(start:))
                return
            end if
            cut = break_point(line, start, start + room - 3)
            if (cut < start) then
                ! No safe break exists: emitting an over-long line is wrong, but
                ! so is splitting a token, and a single unbreakable token is the
                ! generator's problem to report rather than this module's to
                ! silently corrupt.
                call b%line(indent//line(start:))
                return
            end if
            if (start == 1) then
                call b%line(indent//line(start:cut)//" &")
            else
                call b%line(indent//"    "//line(start:cut)//" &")
            end if
            start = cut + 1
            do while (start <= len(line))
                if (line(start:start) /= " ") exit
                start = start + 1
            end do
        end do
    end subroutine put_wrapped

    integer function break_point(line, lo, hi) result(cut)
        !! Last position at or before `hi` that a continuation may follow.
        !!
        !! Whitespace and the end of an opening parenthesis or comma qualify.
        !! In particular, `procedure_name(first_argument` may be longer than a
        !! line even though both identifiers satisfy Fortran's length limit.
        !! Never break inside a quoted literal, `**`, a number or an identifier.
        character(len=*), intent(in) :: line
        integer, intent(in) :: lo, hi
        integer :: i, top
        character :: quote, current

        cut = lo - 1
        top = min(hi, len(line))
        quote = " "
        i = lo
        do while (i <= top)
            current = line(i:i)
            if (quote /= " ") then
                if (current == quote) then
                    if (i < len(line)) then
                        if (line(i + 1:i + 1) == quote) then
                            i = i + 2
                            cycle
                        end if
                    end if
                    quote = " "
                end if
            else
                select case (current)
                case ("'", '"')
                    quote = current
                case (" ")
                    if (i > lo) cut = i - 1
                case ("(", ",")
                    cut = i
                end select
            end if
            i = i + 1
        end do
    end function break_point

    pure function indent_of(levels) result(s)
        !! `levels` of four-space indentation, the house style.
        integer, intent(in) :: levels
        character(len=:), allocatable :: s
        integer :: i

        s = ""
        do i = 1, levels
            s = s//"    "
        end do
    end function indent_of

end module fortgen_layout
