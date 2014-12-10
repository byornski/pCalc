module int_stack
  use bigintops

  !Single node of the stack
  type node
     type(node), pointer :: last
!     integer :: val
     type(bigint) :: val
  end type node

  !A linked list of items. Pointer kept to the top item
  type :: IntStack
     integer :: num_items = 0
     type(node), pointer :: cur
   contains
     procedure :: Push => intstack_push
     procedure :: Pop => intstack_pop
     procedure :: Peek => intstack_peek
  end type IntStack




contains

  !Generate new stack
  subroutine intstack_new(this)
    class(IntStack), intent(inout) :: this
    this%num_items = 0
    nullify(this%cur)
  end subroutine intstack_new


  !Push items on to the stack
  subroutine intstack_push(this,val)
    class(IntStack), intent(inout) :: this
    type(bigint), intent(in) :: val
    type(node), pointer :: newnode
    allocate(newnode)
    newnode%val = val
    !Is this a new stack?
    if (this%num_items.eq.0) then
       nullify(newnode%last)
    else
       newnode%last => this%cur
    end if
    this%cur => newnode
    this%num_items = this%num_items + 1
  end subroutine intstack_push

  !Pop items off the stack
  type(bigint) function intstack_pop(this)
    class(IntStack), intent(inout) :: this
    !Check stack not empty...
    if (this%num_items .eq.0) then
       STOP 'stack empty'
    end if
    intstack_pop = this%cur%val
    this%cur => this%cur%last
    this%num_items = this%num_items - 1
  end function intstack_pop

  !Peek at top item on the stack
  type(bigint) function intstack_peek(this)
    class(IntStack), intent(inout) :: this
    !Check stack not empty...
    if (this%num_items .eq.0) then
       STOP 'stack empty'
    end if
    intstack_peek = this%cur%val
  end function intstack_peek


end module int_stack
