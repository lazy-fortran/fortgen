program test_expr_ir
    use fortgen_expr_ir, only: expr_ir_t, read_expr_ir
    implicit none
    type(expr_ir_t) :: ir, restored
    character(:), allocatable :: why, text
    logical :: ok
    integer :: a,b,c,x,r,unit
    character(1024) :: path
    call ir%init(20)
    a=ir%add('integer','9007199254740993',[integer::],ok,why)
    if (.not. ok) error stop 'large integer'
    b=ir%add('integer','10',[integer::],ok,why)
    c=ir%add('rational','',[a,b],ok,why)
    if (.not. ok) error stop 'rational'
    x=ir%add('symbol',"x'\n",[integer::],ok,why,3,'local')
    if (.not. ok) error stop 'escaped symbol'
    r=ir%add('mul','',[c,x],ok,why)
    if (.not. ok) error stop 'shared expression'
    text=ir%serialize()
    if (command_argument_count()>0) then
        call get_command_argument(1,path)
        open(newunit=unit,file=trim(path),status='replace', &
            access='stream',form='unformatted')
        write(unit) text
        close(unit)
    end if
    call read_expr_ir(text,restored,ok,why)
    if (.not. ok) then
        print *, why
        error stop 'roundtrip parse'
    end if
    if (restored%python(r) /= ir%python(r)) error stop 'exact meaning lost'
    if (restored%nodes(a)%text /= '9007199254740993') error stop 'rounded integer'
    r=ir%add('symbol',"x'\n",[integer::],ok,why,0,'local')
    if (ok) error stop 'scope conflict'
    r=ir%add('pow','',[99,1],ok,why)
    if (ok) error stop 'forward reference'
    call read_expr_ir('fortgen-expr-ir 2'//new_line('a'),restored,ok,why)
    if (ok) error stop 'version accepted'
    call restored%init(0)
    r=restored%add('integer','1',[integer::],ok,why)
    if (ok) error stop 'budget ignored'
    print *, 'exact Expr IR PASS'
end program
