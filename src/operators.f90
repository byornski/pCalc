module operators
  use bigintops


  !Operator types allowed
  type op
     character :: symbol
     integer :: precedence
     logical :: lassoc
     integer :: num_operands
  end type op

  !List of current operators... again a bit hacky
  integer, parameter :: MaxOperators = 10
  integer :: NumOperators = 0
  type(op), dimension(MaxOperators), target :: operator_list


contains

  !Get the index of the operator (if any)
  integer function OperatorNumber(ch)
    character, intent(in) :: ch
    integer :: i
    do i=1,NumOperators
       if (ch .eq. operator_list(i)%symbol) then
          OperatorNumber = i
          return
       end if
    end do
    OperatorNumber = 0
  end function OperatorNumber


  !Is this character an operator?
  logical function IsOperator(ch)
    character, intent(in) :: ch
    IsOperator = OperatorNumber(ch) .gt. 0
  end function IsOperator

  !Apply the 2ary operator to the inputs...... 
  !TODO: just pass in the stack and let it decide based on num_operands how many to take
  type(bigint) function ApplyOp(op_in,a,b)
    type(op), intent(in) :: op_in
    type(bigint), intent(in) :: a,b
    select case(op_in%symbol)
    case('+')
       ApplyOp = a+b
    case('-')
       ApplyOp = a-b
    case('*')
       ApplyOp = a*b
    case('/')
       ApplyOp = a/b
    case('^')
       ApplyOp = a**b
    case('%')
       ApplyOp = BigIntMod(a,b)
    case default
       STOP 'unknown operator'
    end select
  end function ApplyOp


  !Add an operation to the list of known operators
  subroutine AddOperation(symbol,precedence,lassoc,num_operands)
    integer, intent(in) :: precedence
    character, intent(in) :: symbol
    logical, intent(in) :: lassoc
    integer, intent(in) :: num_operands
    NumOperators = NumOperators + 1
    operator_list(NumOperators)%symbol = symbol
    operator_list(NumOperators)%precedence = precedence
    operator_list(NumOperators)%lassoc = lassoc
    operator_list(NumOperators)%num_operands = num_operands
  end subroutine AddOperation
    
  !Operator definitions.... symbol,precedence,left associative,number of operands
  subroutine InitOps
    call AddOperation('+',1,.true.,2)
    call AddOperation('-',1,.true.,2)
    call AddOperation('*',2,.true.,2)
    call AddOperation('/',2,.true.,2)
    call AddOperation('%',2,.true.,2)
    call AddOperation('^',3,.false.,2)
    call AddOperation('(',5,.true.,0)
    call AddOperation(')',5,.true.,0)
  end subroutine InitOps
 



end module operators
  
