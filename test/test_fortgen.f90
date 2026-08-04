program test_fortgen
    !! Behavioural tests for the shared generation conventions.
    !!
    !! The property that matters is not "the output looks tidy" but "the output
    !! is still the same Fortran". Every wrapping test therefore checks that
    !! rejoining the continued lines reproduces the original statement exactly,
    !! and that no break landed inside a token.
    use fortgen_buffer, only: buffer_t
    use fortgen_layout, only: put_wrapped, indent_of, DEFAULT_LINE_LIMIT
    use fortgen_banner, only: put_banner
    implicit none

    integer :: failures

    failures = 0

    call test_buffer_accumulates(failures)
    call test_short_line_untouched(failures)
    call test_wrapping_preserves_text(failures)
    call test_no_break_inside_token(failures)
    call test_unbreakable_token_is_not_split(failures)
    call test_banner(failures)

    if (failures == 0) then
        print *, "test_fortgen: all cases passed"
    else
        print *, "test_fortgen: ", failures, " case(s) FAILED"
        error stop 1
    end if

contains

    subroutine test_buffer_accumulates(failures)
        !! Appending many times must produce simple concatenation.
        integer, intent(inout) :: failures
        type(buffer_t) :: b
        character(len=:), allocatable :: expect
        integer :: i

        expect = ""
        do i = 1, 500
            call b%put("abcdefgh")
            expect = expect//"abcdefgh"
        end do
        call expect_equal("buffer_accumulates", b%str(), expect, failures)
    end subroutine test_buffer_accumulates

    subroutine test_short_line_untouched(failures)
        !! A line inside the limit gets no continuation.
        integer, intent(inout) :: failures
        type(buffer_t) :: b

        call put_wrapped(b, "    ", "z = x + y")
        call expect_equal("short_line_untouched", b%str(), &
                          "    z = x + y"//achar(10), failures)
    end subroutine test_short_line_untouched

    subroutine test_wrapping_preserves_text(failures)
        !! Rejoining the continued lines must reproduce the statement.
        integer, intent(inout) :: failures
        type(buffer_t) :: b
        character(len=:), allocatable :: line

        line = "z_d = a * sin(b) + c * cos(d) + e * tan(f) + g * exp(h) + "// &
               "i * log(j) + k * sqrt(l) + m * tanh(n) + o * atan(p)"
        call put_wrapped(b, "        ", line)
        call expect_equal("wrapping_preserves_text", rejoin(b%str()), line, &
                          failures)
    end subroutine test_wrapping_preserves_text

    subroutine test_no_break_inside_token(failures)
        !! No continuation may fall inside an operator or a literal. Tokens are
        !! separated by spaces in generated code, so every line must end at a
        !! token boundary.
        integer, intent(inout) :: failures
        type(buffer_t) :: b
        character(len=:), allocatable :: text, line
        integer :: i, start, stop_at
        logical :: bad

        line = "value = 1.234567d0 ** 2 * alpha ** 3 + beta ** 4 * "// &
               "gamma ** 5 - delta ** 6 * epsilon ** 7 + zeta ** 8"
        call put_wrapped(b, "    ", line)
        text = b%str()

        bad = .false.
        start = 1
        do i = 1, len(text)
            if (text(i:i) /= achar(10)) cycle
            stop_at = i - 1
            if (stop_at >= start + 1) then
                ! A continued line ends with " &"; the character before the
                ! ampersand must be a space, never half of a token.
                if (text(stop_at:stop_at) == "&") then
                    if (text(stop_at - 1:stop_at - 1) /= " ") bad = .true.
                end if
            end if
            start = i + 1
        end do

        if (bad) then
            print *, "FAIL no_break_inside_token"
            print *, text
            failures = failures + 1
        else
            print *, "pass no_break_inside_token"
        end if
    end subroutine test_no_break_inside_token

    subroutine test_unbreakable_token_is_not_split(failures)
        !! A single token longer than the limit has no safe break. It must be
        !! emitted whole - an over-long line the generator can be told about,
        !! not silent corruption.
        integer, intent(inout) :: failures
        type(buffer_t) :: b
        character(len=:), allocatable :: line

        line = repeat("x", 200)
        call put_wrapped(b, "    ", line)
        call expect_equal("unbreakable_token_is_not_split", rejoin(b%str()), &
                          line, failures)
    end subroutine test_unbreakable_token_is_not_split

    subroutine test_banner(failures)
        !! The banner names the generator and forbids editing.
        integer, intent(inout) :: failures
        type(buffer_t) :: b
        character(len=:), allocatable :: text

        call put_banner(b, "fortad 0.1.0", regenerate="fortad --mode reverse k.f90")
        text = b%str()
        if (index(text, "fortad 0.1.0") == 0 .or. &
            index(text, "Do not edit") == 0 .or. &
            index(text, "--mode reverse") == 0) then
            print *, "FAIL banner"
            print *, text
            failures = failures + 1
        else
            print *, "pass banner"
        end if
    end subroutine test_banner

    function rejoin(text) result(line)
        !! Undo continuation: drop trailing " &", newlines, and leading blanks.
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: line
        character(len=:), allocatable :: piece
        integer :: i, start

        line = ""
        start = 1
        do i = 1, len(text)
            if (text(i:i) /= achar(10)) cycle
            piece = text(start:i - 1)
            start = i + 1
            if (len_trim(piece) == 0) cycle
            piece = trim(adjustl(piece))
            if (len(piece) >= 1) then
                if (piece(len(piece):len(piece)) == "&") then
                    piece = trim(piece(1:len(piece) - 1))
                    line = line//piece//" "
                    cycle
                end if
            end if
            line = line//piece
        end do
    end function rejoin

    subroutine expect_equal(label, got, want, failures)
        !! Compare and report.
        character(len=*), intent(in) :: label, got, want
        integer, intent(inout) :: failures

        if (got == want) then
            print *, "pass ", label
        else
            print *, "FAIL ", label
            print *, "  got:  [", got, "]"
            print *, "  want: [", want, "]"
            failures = failures + 1
        end if
    end subroutine expect_equal

end program test_fortgen
