
module biginttype
  integer, parameter :: intkind=8

  type bigint
     integer(kind=intkind) :: size
     logical :: allocated = .false.
     logical :: assigned = .false.
     integer(kind=intkind), dimension(:), allocatable :: vals
     integer(kind=intkind) :: sign_val
     integer(kind=intkind) :: digits
  end type bigint
end module biginttype
