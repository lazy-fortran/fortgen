program test_expr_ir
    use fortgen_expr_ir, only: expr_ir_t, read_expr_ir
    implicit none
    type(expr_ir_t) :: ir, restored
    character(:), allocatable :: why, text, expected, actual
    logical :: ok
    integer :: a,b,c,x,r,unit,k
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
    expected=ir%python(r,ok,why)
    if (.not. ok) error stop 'render original'
    actual=restored%python(r,ok,why)
    if (.not. ok) error stop 'render restored'
    if (actual/=expected) error stop 'exact meaning lost'
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
    call restored%init(200)
    a=restored%add('integer','1',[integer::],ok,why)
    do k=1,128
        b=restored%add('add','',[a],ok,why)
        if (.not. ok) exit
        a=b
    end do
    if (ok .or. index(why,'depth')==0) error stop 'depth budget ignored'
    call restored%init(100)
    a=restored%add('symbol','x',[integer::],ok,why)
    do k=1,30
        a=restored%add('mul','',[a,a],ok,why)
        if (.not. ok) error stop 'shared DAG construction'
    end do
    text=restored%python(a,ok,why)
    if (ok .or. len(text)/=0) error stop 'exponential rendering budget ignored'
    print *, 'exact Expr IR PASS'
end program
