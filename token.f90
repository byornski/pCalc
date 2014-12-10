module Tokens
  use operators
  use biginttype

  integer, parameter :: tt_operator = 0
  integer, parameter :: tt_value = 1
  integer, parameter :: tt_lparens = 2
  integer, parameter :: tt_rparens = 3
  
  
  type Token
     integer :: TokenType
!     integer :: value
     type(bigint) :: value
     type(op), pointer :: op
  end type Token

  
  
  

end module Tokens
