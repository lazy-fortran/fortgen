module fortgen_buffer
    !! A growable character buffer for source generation.
    !!
    !! Building generated code with repeated `text = text//more` is quadratic:
    !! every concatenation reallocates and copies everything written so far, and
    !! on a kernel with thousands of statements that becomes the dominant cost.
    !! This buffer grows geometrically and materialises the string once.
    !!
    !! It is also the answer to a subtler trap. A recursive function returning
    !! `character(len=:), allocatable` is the obvious way to render a nested
    !! expression, and under gfortran it silently corrupted its own output in
    !! fortad's first emitter. Appending into a buffer through recursive
    !! *subroutines* has no such failure mode.
    implicit none
    private

    public :: buffer_t

    type :: buffer_t
        character(len=:), allocatable :: data
        integer :: length = 0
    contains
        procedure :: put => buffer_put
        procedure :: line => buffer_line
        procedure :: str => buffer_str
        procedure :: reset => buffer_reset
    end type buffer_t

contains

    subroutine buffer_put(self, text)
        !! Append text, growing the store geometrically.
        class(buffer_t), intent(inout) :: self
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: bigger
        integer :: need, cap

        if (len(text) == 0) return
        if (.not. allocated(self%data)) then
            allocate (character(len=1024) :: self%data)
            self%length = 0
        end if
        need = self%length + len(text)
        cap = len(self%data)
        if (need > cap) then
            do while (cap < need)
                cap = 2*cap
            end do
            allocate (character(len=cap) :: bigger)
            bigger(1:self%length) = self%data(1:self%length)
            call move_alloc(bigger, self%data)
        end if
        self%data(self%length + 1:need) = text
        self%length = need
    end subroutine buffer_put

    subroutine buffer_line(self, text)
        !! Append text followed by a newline.
        class(buffer_t), intent(inout) :: self
        character(len=*), intent(in) :: text

        call self%put(text)
        call self%put(new_line('a'))
    end subroutine buffer_line

    function buffer_str(self) result(text)
        !! The accumulated text.
        class(buffer_t), intent(in) :: self
        character(len=:), allocatable :: text

        if (.not. allocated(self%data) .or. self%length == 0) then
            text = ""
        else
            text = self%data(1:self%length)
        end if
    end function buffer_str

    subroutine buffer_reset(self)
        !! Discard the contents, keeping the allocation.
        class(buffer_t), intent(inout) :: self

        self%length = 0
    end subroutine buffer_reset

end module fortgen_buffer
