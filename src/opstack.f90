module operatorstack
  use operators


  !Stack of operators......... bit hacky right now

  type opptr
     type(op), pointer :: p
  end type opptr
  
  integer, parameter :: stacksize = 200
  type(opptr), dimension(stacksize) :: opstack
  integer :: on_stack = 0
  
  
contains

 
  subroutine PushOp(op_in)
    type(op), target, intent(in) :: op_in
    on_stack = on_stack + 1
    if (on_stack .gt. stacksize)  stop 'Stack full...'
    opstack(on_stack)%p => op_in
  end subroutine PushOp

  
  function PopOp()
    type(op), pointer :: PopOp
    if (on_stack .lt. 1) stop 'Stack already empty'
    PopOp => opstack(on_stack)%p
    on_stack = on_stack - 1
  end function PopOp

  function PeekOp()
    type(op), pointer :: PeekOp
    if (on_stack .lt. 1) stop 'Stack already empty'
    PeekOp => opstack(on_stack)%p
  end function PeekOp
  
end module operatorstack
