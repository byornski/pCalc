program pcalc
  use Tokens
  use Operators
  use operatorstack
  use int_stack
  implicit none

  logical, parameter :: debug_flag = .false.
  integer, parameter :: strlen = 200
  character(len=strlen) :: datastr, exename
  integer :: iostat, num_skip_chars, num_chars

  type(bigint) :: ans

  call InitOps()



  !Check if we got anything on the command line...
  if (COMMAND_ARGUMENT_COUNT() .gt. 0) then
     
     call GET_COMMAND(datastr)
     call GET_COMMAND_ARGUMENT(0,exename)

     !Trim off the exe name
     num_chars = len_trim(datastr) 
     num_skip_chars = len_trim(exename) + 2

     datastr = datastr(num_skip_chars:num_chars)
     write(*,*) trim(BigIntStr(ParseStr(datastr)))


  else

     !Otherwise just loop until we reach an exit
     datastr = ""  
     do
  
        write(*,'(A)',advance='no') 'Input : '
        read(*,'(A)', iostat=iostat) datastr
        
        !Exit conditions
        if (iostat.ne.0) then
           write(*,*)
           exit
        end if
        if (trim(datastr).eq.'exit') exit
        if (trim(datastr).eq.'') cycle


        
        write(*,'(A)',advance='no') 'Result: '

        !Now to actually do a calculation....
        ans= ParseStr(datastr)

        write(*,*) trim(BigIntStr(ans))
     end do
  end if

  



  
contains

  !Parse input strings, and return the answer
  type(bigint) function ParseStr(datastr)
    character(len=*) :: datastr
    type(Token), dimension(:), allocatable :: ts    

    !First split into tokens
    call TokenizeString(datastr,ts)    
    
    !Now lets shunting-yard this
    call ShuntYard(ts)

    if (debug_flag) then
       call PrintTokens(ts)
    end if

    !Finally work out the answer by evaluating the RPN
    ParseStr = EvalRPN(ts)

  end function ParseStr

  !Debug function to see RPN form
  subroutine PrintTokens(tks)
    type(Token), dimension(:) :: tks
    integer :: i
    write(*,'(A)',advance='no') 'RPN: '
    do i=1,size(tks)
       select case(tks(i)%tokentype)
       case(tt_value)
          write(*,'(I2)',advance='no') trim(bigintstr(tks(i)%value))
       case(tt_operator)
          write(*,'(A2)',advance='no') tks(i)%op%symbol
       case default
          write(*,*) "??"
       end select
    end do
    write(*,*)
  end subroutine PrintTokens


  !Evaluate the Reverse Polish Notation
  type(bigint) function EvalRPN(tokens)
    type(Token), dimension(:) :: tokens
    type(intstack) :: is
    
    integer :: num_tokens
    integer ::  i
    type(bigint) :: a,b,c

    num_tokens = size(tokens)

    !Loop over tokens...
    do i=1,num_tokens
       if (tokens(i)%tokentype .eq. tt_value) then
         !Value - add to stack
          call is%push(tokens(i)%value)
      else
          !Operator -- perform it
         b = is%pop()
         a = is%pop()
         c = ApplyOp(tokens(i)%op,a,b)
         call is%push(c)
      end if
    end do
    !Top value on stack should be the answer
    evalrpn = is%pop()
  end function EvalRPN


  !Djikstra's shunt yard algorithm to turn infix into postfix
  subroutine ShuntYard(tokens)
    type(Token), dimension(:), allocatable :: tokens
    type(Token), dimension(:), allocatable :: SortedTokens

    type(op), pointer:: tmp_op
    integer :: i
    integer :: opstacksize
    integer :: outtokenscount
    logical :: popvals
    
    opstacksize = 0
    outtokenscount = 0

    nullify(tmp_op)
    allocate(SortedTokens(size(tokens)))

    do i=1,size(tokens)

       select case(tokens(i)%tokentype)
       case(tt_value)
          outtokenscount = outtokenscount + 1
          SortedTokens(outtokenscount) = tokens(i)

       case(tt_operator)
          !Too long to explain -- see wikipedia case for if it's an operator
          do
             if (on_stack .lt. 1) exit

             tmp_op => PeekOp()
             if (tmp_op%symbol .eq. '(') exit 
             popvals = (tokens(i)%op%lassoc .and. tokens(i)%op%precedence .le. tmp_op%precedence) .or. &
                  & ((.not. tokens(i)%op%lassoc) .and. tokens(i)%op%precedence .lt. tmp_op%precedence)

             if (.not. popvals) exit

             !Pop to output
             outtokenscount = outtokenscount + 1
             SortedTokens(outtokenscount)%tokentype  = tt_operator
             tmp_op => PopOp()
             SortedTokens(outtokenscount)%op => tmp_op

          end do

          !Push operator to stack
          call PushOp(tokens(i)%op)

       case(tt_lparens)
          !just push onto the stack
          call PushOp(tokens(i)%op)
       case(tt_rparens)
          !Pop until left parens or empty
          do
             if (on_stack .lt. 1) STOP 'Unmatched parens'
             tmp_op => PopOp()
             if (tmp_op%symbol .eq. '(') exit
             outtokenscount = outtokenscount + 1
             SortedTokens(outtokenscount)%tokentype  = tt_operator
             SortedTokens(outtokenscount)%op => tmp_op
          end do
       case default
          STOP 'Undefined type...?'
       end select
    end do

    !Now just go through the stack popping and adding to the output list
    do i=1,on_stack
       tmp_op => PopOp()
       outtokenscount = outtokenscount + 1
       SortedTokens(outtokenscount)%tokentype = tt_operator
       SortedTokens(outtokenscount)%op => tmp_op
    end do

    !Now reallocate tokens having removed all ()s
    deallocate(tokens)
    allocate(tokens(outtokenscount))
    tokens = SortedTokens(1:outtokenscount)
    
  end subroutine ShuntYard

  
  !Split input string by spaces and 'type' into tokens
  subroutine TokenizeString(strVal,tokens)
    character(len=*), intent(in) :: strVal
    type(Token), dimension(:), allocatable :: tokens
    integer :: wordstart
    integer :: tokencount, i
    
    tokencount = 0
    i=1
    !First count the tokens 
strloop:    do 
       do while(IsSpace(strVal(i:i)))
          !cycle...
          i = i + 1
          if (i.gt.len(strVal)) exit strloop
       end do
              
       !Ok found a nonspace character
       tokencount = tokencount + 1

       if (isOperator(strval(i:i))) then
          i = i + 1
       else
       
          !Now loop until characters are not
          do while(.not. IsSpace(strVal(i:i)) .and. .not. IsOperator(strVal(i:i)))
             i = i + 1
             if (i.gt.len(strVal)) exit strloop
          end do
       end if
    end do strloop

    allocate(tokens(tokencount))

    tokencount = 0
    wordstart = 1
    i=1

strloop2:    do 
       do while(IsSpace(strVal(i:i)))
          !cycle...
          i = i + 1
          if (i.gt.len(strVal)) exit strloop2
       end do
              
       !Ok found a nonspace character
       tokencount = tokencount + 1

       if (isOperator(strval(i:i))) then
          tokens(tokencount) = NewToken(StrVal(i:i))
          i = i + 1
       else
          wordstart = i
          !Now loop until characters are not
          do while(.not. IsSpace(strVal(i:i)) .and. .not. IsOperator(strVal(i:i)))
             i = i + 1
             if (i.gt.len(strVal)) exit strloop2
          end do
          tokens(tokencount) = NewToken(StrVal(wordstart:i-1))
       end if
    end do strloop2  
    
  end subroutine TokenizeString

  !Turn strval into a token of the right type
  type(Token) function NewToken(strVal)
    character(len=*), intent(in) :: strVal
    integer :: opnum
    integer :: i
    logical :: is_numeric

    !Check if numeric
    is_numeric = .true.
    do i=1,len_trim(strval)
       select case(strval(i:i))
       case ('0':'9')
          !do nothing
       case default
          is_numeric = .false.
          exit
       end select
    end do

    opnum = OperatorNumber(strVal(1:1))    
    nullify(NewToken%op)
    
    if(opnum .gt. 0) then
       !We have an operator
       NewToken%op => operator_list(opnum)

       if (NewToken%op%symbol .eq. '(') then
          NewToken%tokentype = tt_lparens
       else if (NewToken%op%symbol .eq. ')') then
          NewToken%tokentype = tt_rparens
       else
          NewToken%tokentype = tt_operator
       end if

    else if (is_numeric) then
       !Read Successfully so value is an integer
       NewToken%tokentype = tt_value
       call BigIntParseStr(NewToken%value,strval)
    else

       write(*,*) strval(1:1)
       !Something went terribly wrong
       STOP 'invalid char...'

    end if

    if (debug_flag) then
       if (NewToken%tokentype .eq. tt_value) then
          write(*,*) strVal, '=> value:', trim(BigIntStr(NewToken%value))
       else if (NewToken%tokentype .eq. tt_operator) then
          write(*,*) strVal, '=> operator:', NewToken%op%symbol
       else
          write(*,*) strVal, '=> parents', NewToken%tokentype .eq. tt_lparens, NewToken%tokentype .eq. tt_rparens
       end if
    end if
  end function NewToken


  logical function IsSpace(ch)
    character :: ch
    IsSpace = (ch.eq.' ')
  end function IsSpace

end program pcalc
