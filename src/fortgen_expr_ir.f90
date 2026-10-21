module fortgen_expr_ir
    ! Exact mathematical interchange, separate from floating Kernel IR.
    use fortgen_string, only: str_t, str, chars
    implicit none
    private
    public :: expr_ir_t, expr_node_t, python_string, exact_integer, read_expr_ir
    type :: expr_node_t
        character(:), allocatable :: tag, text, scope
        integer :: facts = 0, depth = 1
        integer, allocatable :: args(:)
    end type
    type :: expr_ir_t
        type(expr_node_t), allocatable :: nodes(:)
        integer :: n = 0, max_nodes = 10000
    contains
        procedure :: init
        procedure :: add
        procedure :: python
        procedure :: serialize
    end type
contains
    subroutine read_expr_ir(text, ir, ok, why)
        character(*), intent(in) :: text
        type(expr_ir_t), intent(out) :: ir
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: tokens(69)
        character(:), allocatable :: line, token
        integer :: first, last, p, q, count, status, serial, facts, id, k
        integer :: args(64)
        call ir%init()
        ok = .false.
        why = 'exact Expr IR: unknown version or malformed transcript'
        last = index(text,new_line('a'))
        if (last == 0) return
        if (text(:last-1) /= 'fortgen-expr-ir 1') return
        first = last+1
        do while (first <= len(text))
            ok=.false.
            why="exact Expr IR: malformed transcript"
            last = index(text(first:),new_line('a'))
            if (last == 0) return
            last = first+last-2
            line = text(first:last)
            first = last+2
            p = 1
            count = 0
            do while (p <= len(line))
                if (line(p:p) == ' ') then
                    p=p+1
                    cycle
                end if
                count=count+1
                if (count > size(tokens)) return
                if (line(p:p) == "'") then
                    token=''
                    p=p+1
                    do
                        if (p > len(line)) return
                        if (line(p:p) == "'") exit
                        if (iachar(line(p:p)) == 92) then
                            if (p+3 > len(line)) return
                            if (line(p+1:p+1) /= 'x') return
                            read(line(p+2:p+3),'(z2)',iostat=status) k
                            if (status /= 0) return
                            if (k < 0 .or. k > 127) return
                            token=token//achar(k)
                            p=p+4
                        else
                            token=token//line(p:p)
                            p=p+1
                        end if
                    end do
                    p=p+1
                else
                    q=index(line(p:),' ')
                    if (q == 0) q=len(line)-p+2
                    token=line(p:p+q-2)
                    p=p+q-1
                end if
                tokens(count)=str(token)
            end do
            if (count < 5) return
            token=chars(tokens(1))
            if (.not. exact_integer(token)) return
            read(token,*,iostat=status) serial
            if (status /= 0) return
            if (serial /= ir%n+1) return
            token=chars(tokens(5))
            if (.not. exact_integer(token)) return
            read(token,*,iostat=status) facts
            if (status /= 0) return
            do k=6,count
                token=chars(tokens(k))
                if (.not. exact_integer(token)) return
                read(token,*,iostat=status) args(k-5)
                if (status /= 0) return
            end do
            id=ir%add(chars(tokens(2)),chars(tokens(3)),args(:count-5), &
                ok,why,facts,chars(tokens(4)))
            if (.not. ok) return
        end do
        ok=.true.
        why=''
    end subroutine

    subroutine init(self, budget)
        class(expr_ir_t), intent(inout) :: self
        integer, optional, intent(in) :: budget
        if (present(budget)) self%max_nodes = max(0, budget)
        if (allocated(self%nodes)) deallocate(self%nodes)
        allocate(self%nodes(self%max_nodes))
        self%n = 0
    end subroutine

    logical function exact_integer(text) result(ok)
        character(*), intent(in) :: text
        integer :: k, first
        ok = .false.
        if (len(text) == 0) return
        first = 1
        if (text(1:1) == '-') first = 2
        if (first > len(text)) return
        if (len(text) > first .and. text(first:first) == '0') return
        if (text == '-0') return
        do k = first, len(text)
            if (text(k:k) < '0' .or. text(k:k) > '9') return
        end do
        ok = .true.
    end function

    integer function add(self, tag, text, args, ok, why, facts, scope) result(id)
        class(expr_ir_t), intent(inout) :: self
        character(*), intent(in) :: tag, text
        integer, intent(in) :: args(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer, optional, intent(in) :: facts
        character(*), optional, intent(in) :: scope
        integer :: k, f, depth
        logical :: positive, nonnegative, nonpositive, nonzero, zero
        character(:), allocatable :: domain
        id = 0
        ok = .false.
        why = 'exact Expr IR: malformed node or exhausted budget'
        if (.not. allocated(self%nodes)) call self%init()
        if (self%n >= self%max_nodes) return
        if (any(args < 1) .or. any(args > self%n)) return
        if (len(text) > 65536) return
        f = 0
        if (present(facts)) f = facts
        if (f < 0 .or. f > 2047) return
        if (size(args) > 64) return
        depth=1
        do k=1,size(args)
            depth=max(depth,self%nodes(args(k))%depth+1)
        end do
        if (depth>128) then
            why='exact Expr IR: depth budget exhausted'
            return
        end if
        if (any([(iachar(text(k:k)) > 127, k=1,len(text))])) return
        domain = ''
        if (present(scope)) domain = scope
        if (len(domain)>65536) return
        if (any([(iachar(domain(k:k))>127,k=1,len(domain))])) return
        select case(tag)
        case('integer')
            if (.not. exact_integer(text) .or. size(args) /= 0) return
        case('rational')
            if (size(args) /= 2 .or. len(text) /= 0) return
            do k=1,2
                if (self%nodes(args(k))%tag /= 'integer') return
            end do
            if (self%nodes(args(2))%text == '0') return
        case('symbol')
            do k=1,self%n
                if (self%nodes(k)%tag /= 'symbol') cycle
                if (self%nodes(k)%scope /= domain) cycle
                if (self%nodes(k)%text /= text) cycle
                if (self%nodes(k)%facts /= f) then
                    why = 'exact Expr IR: conflicting symbol scope/assumptions'
                    return
                end if
            end do
            if (len(text) == 0 .or. size(args) /= 0) return
            positive=btest(f,1) .or. btest(f,5)
            nonnegative=btest(f,2) .or. positive .or. btest(f,6)
            nonpositive=btest(f,8) .or. btest(f,7) .or. btest(f,6)
            nonzero=btest(f,3) .or. positive .or. btest(f,7)
            zero=btest(f,6) .or. (nonnegative .and. nonpositive)
            if (positive .and. nonpositive) return
            if (btest(f,7) .and. nonnegative) return
            if (zero .and. nonzero) return
        case('constant')
            if (size(args) /= 0) return
            select case(text)
            case('pi','E','I','oo','zoo','nan')
            case default
                return
            end select
        case('add','mul')
            if (size(args) < 1 .or. len(text) /= 0) return
        case('pow')
            if (size(args) /= 2 .or. len(text) /= 0) return
        case('function','uninterpreted')
            if (len(text) == 0) return
            if (tag=='function') then
                select case(text)
                case('sin','cos','tan','exp','log','sqrt','Abs','sign','conjugate')
                    if (size(args)/=1) return
                end select
            end if
        case default
            why = 'exact Expr IR: unsupported node '//tag
            return
        end select
        if (tag /= 'symbol' .and. f /= 0) return
        self%n = self%n+1
        id = self%n
        self%nodes(id)%tag = tag
        self%nodes(id)%text = text
        self%nodes(id)%args = args
        self%nodes(id)%facts = f
        self%nodes(id)%depth = depth
        self%nodes(id)%scope = ''
        if (present(scope)) self%nodes(id)%scope = scope
        ok = .true.
        why = ''
    end function

    function python_string(text) result(quoted)
        character(*), intent(in) :: text
        character(:), allocatable :: quoted
        character(4) :: escape
        integer :: k, code
        quoted = "'"
        do k=1,len(text)
            code = iachar(text(k:k))
            if (code < 32 .or. code > 126 .or. code == 39 .or. code == 92) then
                write(escape,'(a,z2.2)') achar(92)//'x',code
                quoted = quoted//escape
            else
                quoted = quoted//text(k:k)
            end if
        end do
        quoted = quoted//"'"
    end function

    function python(self,id,ok,why) result(text)
        class(expr_ir_t), intent(in) :: self
        integer, intent(in) :: id
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        character(:), allocatable :: text
        integer :: remaining
        remaining=10000
        ok=.true.
        why=''
        text=render(self,id,remaining,ok,why)
        if (.not. ok) text=''
    end function

    recursive function render(self, id, remaining, ok, why) result(text)
        class(expr_ir_t), intent(in) :: self
        integer, intent(in) :: id
        integer, intent(inout) :: remaining
        logical, intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        character(:), allocatable :: text, child, head
        character(16), parameter :: names(11) = [character(16) :: &
            'real','positive','nonnegative','nonzero','integer','positive', &
            'zero','negative','nonpositive','rational','algebraic']
        integer :: k
        text = ''
        if (id < 1 .or. id > self%n .or. remaining<=0) then
            ok=.false.
            why='exact Expr IR: invalid node or render visit budget exhausted'
            return
        end if
        remaining=remaining-1
        select case(self%nodes(id)%tag)
        case('integer')
            text = 'sp.Integer('//python_string(self%nodes(id)%text)//')'
            return
        case('constant')
            text = 'sp.'//self%nodes(id)%text
            return
        case('symbol')
            child=self%nodes(id)%text
            if (self%nodes(id)%scope/='') then
                block
                    character(32) :: length_text
                    write(length_text,'(i0)') len(self%nodes(id)%scope)
                    child='@'//trim(length_text)//':'//self%nodes(id)%scope//':'//child
                end block
            end if
            text = 'sp.Symbol('//python_string(child)
            do k=1,11
                if (.not. btest(self%nodes(id)%facts,k-1)) cycle
                if (k == 6) then
                    if (.not. btest(self%nodes(id)%facts,1)) text=text//', positive=True'
                    if (.not. btest(self%nodes(id)%facts,4)) text=text//', integer=True'
                else
                    text=text//', '//trim(names(k))//'=True'
                end if
            end do
            text=text//')'
            return
        case('rational'); head='sp.Rational'
        case('add'); head='sp.Add'
        case('mul'); head='sp.Mul'
        case('pow'); head='sp.Pow'
        case('uninterpreted')
            head='sp.Function('//python_string(self%nodes(id)%text)//')'
        case('function')
            select case(self%nodes(id)%text)
            case('sin','cos','tan','exp','log','sqrt','Abs','sign','conjugate')
                head='sp.'//self%nodes(id)%text
            case default
                head='sp.Function('//python_string(self%nodes(id)%text)//')'
            end select
        end select
        text=head//'('
        do k=1,size(self%nodes(id)%args)
            if (k>1) text=text//', '
            child=render(self,self%nodes(id)%args(k),remaining,ok,why)
            if (.not. ok) return
            if (len(text)+len(child)>1024*1024) then
                ok=.false.
                why="exact Expr IR: render byte budget exhausted"
                return
            end if
            text=text//child
        end do
        text=text//')'
    end function

    function serialize(self) result(text)
        class(expr_ir_t), intent(in) :: self
        character(:), allocatable :: text
        character(32) :: number
        integer :: i,k
        text='fortgen-expr-ir 1'//new_line('a')
        do i=1,self%n
            write(number,'(i0)') i
            text=text//trim(number)//' '//self%nodes(i)%tag//' '// &
                python_string(self%nodes(i)%text)//' '// &
                python_string(self%nodes(i)%scope)
            write(number,'(i0)') self%nodes(i)%facts
            text=text//' '//trim(number)
            do k=1,size(self%nodes(i)%args)
                write(number,'(i0)') self%nodes(i)%args(k)
                text=text//' '//trim(number)
            end do
            text=text//new_line('a')
        end do
    end function
end module
