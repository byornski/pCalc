Module bigintops
  use biginttype
  implicit none

!  integer, parameter :: intkind=8

  integer(kind=intkind), parameter :: base = 10000000
  integer(kind=intkind), parameter :: default_size = 20
  integer(kind=intkind), parameter :: buffer_size = 10
  integer(kind=intkind), parameter :: dp = 8
  real(kind=dp), parameter :: tinyval = 1d-10 + 1


  !Division is fastest when done to a small base...
  integer(kind=intkind), parameter :: div_base = 10


  interface operator (+)
     module procedure BigIntAdd
     module procedure BigIntAddInt
     module procedure BigIntAddInt2
  end interface operator (+)

  interface operator (-)
     module procedure BigIntSubtract
  end interface operator (-)

  interface operator (*)
     module procedure BigIntMultiply
     module procedure BigIntMultiplyIntS
  end interface operator (*)

  interface operator (/)
     module procedure BigIntDiv
  end interface operator (/)

  interface operator (**)
     module procedure BigIntPow
  end interface



  interface operator(.eq.)
     module procedure BigIntEquals
     module procedure BigIntEqualsInt
  end interface operator(.eq.)

  interface operator(.gt.)
     module procedure BigIntGreaterThan
  end interface operator(.gt.)

  interface operator(.ge.)
     module procedure BigIntGE
  end interface operator(.ge.)

  interface operator(.lt.)
     module procedure BigIntLessThan
  end interface operator(.lt.)

  interface operator(.le.)
     module procedure BigIntLE
  end interface operator(.le.)

  
  interface assignment(=)
     module procedure BigIntSet
     module procedure BigIntParseStr
  end interface assignment(=)


contains

  

!--------------------------------------------------------------------
!--------------------------------------------------------------------
!                     ARITHMETIC FUNCTIONS
!--------------------------------------------------------------------
!--------------------------------------------------------------------

!--------------------------------------------------------------------
!                          Pow
!--------------------------------------------------------------------
  recursive type(bigint) function BigIntPow(b,pow) result(bi_pow)
    type(bigint), intent(in) :: b,pow
    type(bigint) :: b2

    b2 = 2_intkind

    if (pow .eq. 0_intkind) then
       bi_pow = 1_intkind
    else if (pow.eq.1_intkind) then
       bi_pow = b
    else if (BigIntEven(pow)) then
       bi_pow = BigIntPow(b*b,pow/b2)
    else
       bi_pow = b * BigIntPow(b*b,pow/b2)
    end if

  end function BigIntPow

!--------------------------------------------------------------------
!                          Mod
!--------------------------------------------------------------------


  type(bigint) function BigIntMOD(bi1,bi2) result(bi_mod)
    type(bigint), intent(in) :: bi1, bi2
    type(bigint) :: bi_div

    bi_div = bi1 / bi2

    bi_mod = bi1 - (bi_div * bi2)

  end function BigIntMOD


!--------------------------------------------------------------------
!                          Division
!--------------------------------------------------------------------

  type(bigint) function BigIntDiv(bi1,bi2) result(bi_div)
    type(bigint), intent(in) :: bi1, bi2
    type(bigint) :: remainder
    type(bigint),dimension(:),allocatable :: divisor_list, power_list
    type(bigint) :: bi_tmp

    integer(kind=intkind) :: num_divisors , i, thissize, pcount

    !Set divisors list to something like (base/div_base) * (bi1%digits/bi2%digits) + 1
    integer(kind=intkind) :: divisors_max

    if (bi2.eq.0_intkind) then
       STOP 'division by 0'
    end if

    if (bi2.gt.bi1) then
       bi_div = 0_intkind
       return
    end if

    if (bi1.eq.bi2) then
       bi_div = 1_intkind
       return
    end if

    call BigIntAllocate(bi_div,bi1%size+10)
    call BigIntAllocate(bi_tmp,bi1%size+10)


    num_divisors = CEILING(ApproxLogx(bi1,div_base)-ApproxLogx(bi2,div_base)) + 1
    allocate(divisor_list(num_divisors))
    allocate(power_list(num_divisors))
    
    divisor_list(1) = bi2
    
    power_list(1) = 1_intkind
    


    do i=2,num_divisors
       thissize = bi2%digits + (((bi1%digits - bi2%digits) * i) / num_divisors) + buffer_size
       call BigIntAllocate(divisor_list(i),thissize)
    end do

    do i=2,num_divisors
       thissize = bi2%digits + (((bi1%digits - bi2%digits) * i) / num_divisors) + buffer_size
       call BigIntAllocate(power_list(i),thissize)
    end do

    do i=2,num_divisors
       thissize = bi2%digits + (((bi1%digits - bi2%digits) * i) / num_divisors) + buffer_size

       call BigIntMultiplyInPlace(power_list(i-1),div_base,0_intkind,power_list(i))
       call BigIntMultiplyInPlace(divisor_list(i-1),div_base,0_intkind,divisor_list(i))
       if (divisor_list(i) .gt. bi1) then
          divisors_max = i
          exit
       end if
    end do

    remainder = bi1


    !Now we have our list of bi2*divbase**(i-1)
    do i=num_divisors,1,-1
       pcount = 0
       !Subtract this until remainder .lt. divisors_list(i)
       do while (remainder .ge.  divisor_list(i))
          !Subtract from remainder
          call BigIntSubtractInPlace(remainder,divisor_list(i))
          pcount = pcount + 1
       end do
       
       if (pcount .gt. 0) then
          call BigIntMultiplyInPlace(power_list(i),pcount,0_intkind,bi_tmp)
          call BigIntAddInPlace(bi_div,bi_tmp)
       end if

    end do

  end function BigIntDiv


!--------------------------------------------------------------------
!                    SUBTRACT STUB
!--------------------------------------------------------------------


  type(bigint) function BigIntSubtract(b1,b2) result(bi_sub)
    type(bigint), intent(in) :: b1,b2

    if (.not. b1%allocated) then
       STOP 'BigIntSubtract: b1 not allocated'
    end if

    if (.not. b2%allocated) then
       STOP 'BigIntSubtract: b2 not allocated'
    end if




    !Check Signs.....

    if (b1%sign_val .eq. b2%sign_val) then
       !Both +ve or -ve so do subtraction
       if (b1.ge.b2) then
          bi_sub = BigIntSubtractBigger(b1,b2)
          bi_sub%sign_val = b1%sign_val
       else
          bi_sub = BigIntSubtractBigger(b2,b1)
          bi_sub%sign_val = -1 * b1%sign_val
       end if
    else
       !This is really an add
       bi_sub = BigIntAddBigger(b1,b2)
       bi_sub%sign_val = b1%sign_val
    end if
  end function BigIntSubtract


!--------------------------------------------------------------------
!                    SUBTRACT INPLACE
!--------------------------------------------------------------------

  subroutine BigIntSubtractInplace(b1,b2)
    type(bigint), intent(inout) :: b1
    type(bigint), intent(in) :: b2
    integer(kind=intkind) :: i, carry, sumval

    carry = 0

    !b1 > b2 so result should be +ve
    do i=1,b1%digits
       sumval = b1%vals(i) + carry
       if (i.le.b2%digits) sumval = sumval - b2%vals(i)
       
       if (sumval .lt. 0) then
          !Carry required, need to move result +ve
          carry = -1
          b1%vals(i) = sumval + base
       else
          !No carry, result stands
          carry = 0
          b1%vals(i) = sumval
       end if
    end do
    call CountDigits(b1,b1%digits+1)


  end subroutine BigIntSubtractInplace


!--------------------------------------------------------------------
!                    SUBTRACT WORKHORSE
!--------------------------------------------------------------------

  type(bigint) function BigIntSubtractBigger(b1,b2) result (bi_sub)
    type(bigint), intent(in) :: b1,b2
    integer(kind=intkind) :: i, carry, sumval

    call BigIntAllocate(bi_sub,b1%size)

    !allocate it
    bi_sub = 0_intkind
    carry = 0

    !b1 > b2 so result should be +ve
    do i=1,b1%digits
       sumval = b1%vals(i) + carry
       if (i.le.b2%digits) sumval = sumval - b2%vals(i)
       
       if (sumval .lt. 0) then
          !Carry required, need to move result +ve
          carry = -1
          bi_sub%vals(i) = sumval + base
       else
          !No carry, result stands
          carry = 0
          bi_sub%vals(i) = sumval
       end if
    end do
    call CountDigits(bi_sub,b1%digits+1)

  end function BigIntSubtractBigger

!--------------------------------------------------------------------
!                   MULTIPLY IN PLACE (FAST)
!--------------------------------------------------------------------

  subroutine BigIntMultiplyInPlace(bi1,int_in, lshift, bi_mul)
    !In this version, we assume bi_mul is already allocated
    type(bigint), intent(inout) :: bi_mul
    type(bigint), intent(in) :: bi1
    integer(kind=intkind), intent(in) :: int_in
    integer(kind=intkind), intent(in) :: lshift
    integer(kind=intkind) :: i
    integer(kind=intkind) :: carry, tmpsum
    integer(kind=intkind) :: int

    int = abs(int_in)

    !Check size just in case....
    if (bi1%digits + ceiling(logbase(int)) + lshift  .gt. bi_mul%size) then
       STOP 'slow..... enlarge multiply'
    end if

    bi_mul = 0_intkind

    if (int .eq. 0) return
    
    carry =0

    do i=1,bi1%digits + ceiling(logbase(int))
       if (i.le.bi1%digits) then
          tmpsum = carry + bi1%vals(i) * int
       else
          tmpsum = carry
       end if
       bi_mul%vals(i+lshift) = mod(tmpsum,base)
       carry = tmpsum / base
    end do

    call CountDigits(bi_mul,(ceiling(logbase(int)) + bi1%digits + lshift) + 2)


    bi_mul%sign_val = sign(1_intkind,int_in) * bi1%sign_val

  end subroutine BigIntMultiplyInPlace
  



!--------------------------------------------------------------------  
!                     MULTIPLY INT STUB
!--------------------------------------------------------------------


  type(bigint) function BigIntMultiplyIntS(bi1,int) result (bi_mul)
    type(bigint), intent(in) :: bi1
    integer(kind=intkind), intent(in) :: int
    bi_mul = BigIntMultiplyInt(bi1,int)
  end function BigIntMultiplyIntS

!--------------------------------------------------------------------
!                      MULTIPLY INT (bigint,int) 
!--------------------------------------------------------------------

  type(bigint) function BigIntMultiplyInt(bi1,int, shift) result(bi_mul)
    type(bigint), intent(in) :: bi1
    integer(kind=intkind), intent(in) :: int
    integer(kind=intkind), intent(in), optional :: shift
    integer(kind=intkind) :: lshift, finaldigits

    if (present(shift)) then
       lshift = shift
    else
       lshift = 0
    end if

    finaldigits = (ceiling(logbase(int)) + bi1%digits + lshift)
    if (finaldigits+1 .gt. bi1%size) then
       !enlarge if necessary
       call BigIntAllocate(bi_mul,finaldigits+buffer_size)
    else
       !Try to stay the same size
       call BigIntAllocate(bi_mul,bi1%size)
    end if
    call BigIntMultiplyInPlace(bi1,int, lshift, bi_mul)

  end function BigIntMultiplyInt

!--------------------------------------------------------------------
!                  MULTIPLY (bigint, bigint)             
!--------------------------------------------------------------------

  type(bigint) function BigIntMultiply(bi1,bi2) result(bi_mul)
    type(bigint), intent(in) :: bi1, bi2
    type(bigint) :: bi_tmp
    integer(kind=intkind) :: i
    integer(kind=intkind) :: max_digits, max_size

    if (.not. bi1%allocated) then
       STOP 'BigIntMultiply: bi1 not allocated'
    end if
    if (.not. bi2%allocated) then
       STOP 'BigIntMultiply: bi2 not allocated'
    end if

    max_size = max(bi1%size, bi1%size)
    max_digits = max(bi1%digits,bi2%digits)

    if (max_size .lt. (bi1%digits + bi2%digits)) then
       max_size = (bi1%digits-1 + bi2%digits-1) + buffer_size
    end if
    
    call BigIntAllocate(bi_mul,max_size)
    call BigIntAllocate(bi_tmp,max_size)

    bi_mul = 0_intkind

    do i=1,bi1%digits
       if (bi1%vals(i) .eq. 0) cycle

       !Create b1(i) * bi2
       call BigIntAssign(bi_tmp,0_intkind)
       call BigIntMultiplyInplace(bi2,bi1%vals(i),i-1,bi_tmp)

       !Add to result
       call BigIntAddInPlace(bi_mul, bi_tmp)
    end do
    deallocate(bi_tmp%vals)

 

    bi_mul%sign_val = bi1%sign_val * bi2%sign_val

  end function BigIntMultiply

!--------------------------------------------------------------------
!             ADD IN PLACE (bigint,bigint)
!--------------------------------------------------------------------

  subroutine BigIntAddInPlace(bi1,bi2)
    type(bigint), intent(in) :: bi2
    type(bigint), intent(inout) :: bi1
    integer(kind=intkind) :: max_digits, max_size
    integer(kind=intkind) :: i, sumval, carry

    !bi1 better be >= bi2
    if (bi1%size .lt. bi2%digits) then
       write(*,*) 'enlarging hack in addinplace'
       !ENLARGE
       bi1 = BigIntAdd(bi1,bi2)
       return
    end if  

    max_digits = max(bi1%digits,bi2%digits)
    max_size = bi1%size
    carry = 0

!    write(*,*) max_digits, bi1%digits, bi2%digits

   do i=1,max_digits
      sumval = carry
      if (i .le. bi1%size) sumval = sumval + bi1%vals(i)
      if (i .le. bi2%size) sumval = sumval + bi2%vals(i)
      bi1%vals(i) = mod(sumval,base)
      carry = sumval / base
   end do

   if (carry.ne.0) then
      if (max_digits.eq.max_size) then
         write(*,*) 'OVERFLOW IN ADD : SHOULD NEVER GET THIS'
         return
      end if

      bi1%vals(max_digits+1) = carry
      bi1%digits = max_digits+1
   else
      bi1%digits = max_digits
   end if
  end subroutine BigIntAddInPlace


!--------------------------------------------------------------------
!                     ADD INT (bigint,int)
!--------------------------------------------------------------------


  type(bigint) function BigIntAddInt(bi1,int) result(bi_sum)
    type(bigint), intent(in) :: bi1
    integer(kind=intkind), intent(in) :: int

    call bigintassign(bi_sum,int)
    call BigIntAddInPlace(bi_sum,bi1)


  end function BigIntAddInt


  type(bigint) function BigIntAddInt2(int,bi1) result(bi_sum)
    type(bigint), intent(in) :: bi1
    integer(kind=intkind), intent(in) :: int

    call bigintassign(bi_sum,int)
    call BigIntAddInPlace(bi_sum,bi1)

  end function BigIntAddInt2

!--------------------------------------------------------------------
!                     ADD STUB (bigint,bigint)
!--------------------------------------------------------------------

  type(bigint) function BigIntAdd(b1,b2) result(bi_sum)
    type(bigint), intent(in) :: b1, b2

    if (.not. b1%allocated) then
      STOP 'BigIntAdd: b1 not allocated'
   end if

   if (.not. b2%allocated) then
      STOP 'BigIntAdd: b2 not allocated'
   end if


   !Check Signs.....

   if (b1%sign_val .eq. b2%sign_val) then
      !Both +ve or -ve so addition
         bi_sum = BigIntAddBigger(b1,b2)
         bi_sum%sign_val = b1%sign_val
   else
      !This is really a subtraction
       !Both +ve or -ve so do subtraction
      if (b1.ge.b2) then
         bi_sum = BigIntSubtractBigger(b1,b2)
         bi_sum%sign_val = b1%sign_val
      else
         bi_sum = BigIntSubtractBigger(b2,b1)
         bi_sum%sign_val = -1 * B1%sign_val
      End If

   end if



 end function BigIntAdd


!--------------------------------------------------------------------
!                     ADD (bigint,bigint)
!--------------------------------------------------------------------

  type(bigint) function BigIntAddBigger(bi1,bi2) result(bi_sum)
    type(bigint), intent(in) :: bi1, bi2
    integer(kind=intkind) :: max_digits
    integer(kind=intkind) :: max_size
    integer(kind=intkind) :: i
    integer(kind=intkind) :: carry
    integer(kind=intkind) :: sumval



    if (.not. bi1%allocated) then
       STOP 'BigIntAdd: bi1 not allocated'
    end if
    
    if (.not. bi2%allocated) then
       STOP 'BigIntAdd: bi2 not allocated'
    end if

    max_size = max(bi1%size, bi2%size)
    max_digits = max(bi1%digits,bi2%digits) 

    if (max_size .le. max_digits + 2) max_size = max_size + buffer_size
    call BigIntAllocate(bi_sum,max_size)

    carry = 0
    do i=1,max_digits
       sumval = carry
       if (i .le. bi1%size) sumval = sumval + bi1%vals(i)
       if (i .le. bi2%size) sumval = sumval + bi2%vals(i)
       bi_sum%vals(i) = mod(sumval,base)
       carry = sumval / base
    end do

    if (carry.ne.0) then
       if (max_digits.eq.max_size) then
          write(*,*) 'OVERFLOW IN ADD : SHOULD NEVER GET THIS'
          return
       end if
       bi_sum%vals(max_digits+1) = carry
       bi_sum%digits = max_digits+1
    else
       bi_sum%digits = max_digits
    end if
  end function BigIntAddBigger





!--------------------------------------------------------------------
!--------------------------------------------------------------------
!                           UTILITY
!--------------------------------------------------------------------
!--------------------------------------------------------------------



  subroutine CountDigits(bi, guess)
    type(bigint), intent(inout) :: bi
    integer(kind=intkind), intent(in) :: guess
    integer(kind=intkind) :: i,actualguess

    actualguess = min(guess,bi%size)

    !count down from guess
    do i=actualguess,1,-1
       if (bi%vals(i) .ne. 0) then
          bi%digits = i
          return
       end if
    end do

    do i=bi%size,1,-1
       if (bi%vals(i) .ne. 0) then
          bi%digits = i
          return
       end if
    end do

    bi%digits = 1 
    

  end subroutine CountDigits




  subroutine BigIntSet(bi,val)
    type(bigint), intent(inout) :: bi
    integer(kind=intkind), intent(in) :: val

    call BigIntAssign(bi,val)

  end subroutine BigIntSet


  subroutine BigIntAssign(bi,val_in,size)
    integer(kind=intkind), intent(in) :: val_in
    integer(kind=intkind), intent(in), optional :: size
    type(bigint), intent(inout) :: bi
    integer(kind=intkind) :: ndigits, i, tmp_val
    integer(kind=intkind) :: actual_length, val



    val = abs(val_in)

    if (.not. bi%allocated) then
       if (present(size)) then
          call BigIntAllocate(bi,size)
       else
          actual_length = CEILING(logbase(val))+buffer_size
          call BigIntAllocate(bi,max(default_size, actual_length))
       end if
    end if


    bi%sign_val = sign(1_intkind,val_in)

    !count digits in val
    if (val .eq. 0) then
       bi%digits = 1
       bi%vals = 0
       return
    end if
    
    ndigits = ceiling(logbase(val))+ 1

    if (bi%size .lt. ndigits) then
       write(*,*) 'too big for me', val
       return
    end if

    bi%vals = 0
    bi%digits = ndigits
    
    tmp_val = val

    do i=1,ndigits
       bi%vals(i) = mod(tmp_val,base)
       tmp_val = tmp_val / base
    end do

    call CountDigits(bi,ndigits+2)


  end subroutine BigIntAssign


  subroutine BigIntParseStr(bi,str)
    type(bigint), intent(inout) :: bi
    character(len=*), intent(in) :: str
    character(len=:), allocatable :: one_digit_str
    integer(kind=intkind) :: strlen, chars_per_digit, i, digits
    integer(kind=intkind) :: sStart, sEnd, offset, thisint

    strlen = len_trim(str)

    !ASSUME BASE 10 for string and base 10**n for bigint
    chars_per_digit = NINT(1d0/logbase(10_intkind))
    digits = CEILING(dble(strlen) * logbase(10_intkind) - 1d-6)
    offset = chars_per_digit * digits - strlen
    

    call BigIntAllocate(bi,digits)

    allocate(character(chars_per_digit) :: one_digit_str)

    bi%digits = digits


    do i=digits,1,-1
       !Parse these digits...
       sEnd = i * chars_per_digit - offset
       sStart = max(sEnd - chars_per_digit + 1,1)
       
       
       one_digit_str = str(sStart:sEnd)

       read(one_digit_str,*) thisint

!       write(*,*) i, str(sStart:sEnd), thisint
       bi%vals(digits - i + 1) = thisint
    end do

  end subroutine BigIntParseStr
    



  subroutine BigIntAllocate(bi,size)
    integer(kind=intkind), intent(in), optional :: size
    type(bigint), intent(inout) :: bi
    integer(kind=intkind) :: nsize
    integer :: AllocateStatus

    nsize = default_size
    if (present(size)) nsize = size


    if (bi%allocated) then
       write(*,*) 'already allocated!'
       deallocate(bi%vals)
    end if

 

    allocate(bi%vals(nsize),STAT = AllocateStatus)
    IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

    bi%size = nsize
    bi%digits = 0
    bi%allocated = .true.
    bi%vals = 0
    bi%sign_val = 1
  end subroutine BigIntAllocate



  real(kind=dp) function ApproxLogx(b,x)
    type(bigint), intent(in) :: b
    integer(kind=intkind), intent(in) :: x

!    ApproxLogx = b%digit / logbase(x)

    !Try and get fractional bit from highest digits
    ApproxLogx = log(real(b%vals(b%digits),dp)) / log(real(x,dp))

    ApproxLogx = ApproxLogx + ((b%digits-1) / logbase(x)) 


  end function ApproxLogx


  real(kind=dp) function logbase(val)
    integer(kind=intkind) :: val
    real(kind=dp), parameter :: log_base = log(real(base,dp))
    
    logbase = log(real(val,dp))/log_base

  end function logbase










  character(20000) function BigIntStr(bi)
    type(bigint), intent(in) :: bi
    integer(kind=intkind) :: i
    character(len=100) :: n
    integer(kind=intkind) :: digits_per_val
    character(len=5) :: tmp
    character(len=10) :: fmt, fmt_last

    BigIntStr = ""
    digits_per_val = CEILING(real(1,dp) / logbase(10_intkind))

    !Assume base is a power of 10.....
    write(tmp,"(I4)") digits_per_val

    write(fmt,*) "(I" // trim(adjustl(tmp)) // "." // trim(adjustl(tmp)) // ")"
    write(fmt_last,*) "(I" // trim(adjustl(tmp)) // ")"

    do i=1,bi%digits-1
       write(n,fmt) bi%vals(i)
       BigIntStr = trim(adjustl(n)) // BigIntStr
    end do

    write(n,fmt_last) bi%vals(bi%digits) *  bi%sign_val

    BigIntStr = trim(adjustl(n)) // BigIntStr

  end function BigIntStr

  integer(kind=intkind) function DigitSum(bi)
    type(bigint), intent(in) :: bi
    integer(kind=intkind) :: count, i, X
    count = 0

    do i=1,bi%digits
       !get this chunk of digits
       X = bi%vals(i)
       do
          if (X .eq. 0) exit
          count = count + mod(X,10)
          X = X / 10
       end do
    end do
    
    DigitSum = count
  end function DigitSum



!--------------------------------------------------------------------
!--------------------------------------------------------------------
!                     LOGICAL FUNCTIONS
!--------------------------------------------------------------------
!--------------------------------------------------------------------

  logical function BigIntEven(bi1)
    type(bigint), intent(in) :: bi1
    BigIntEven = mod(bi1%vals(1),2).eq.0
  end function BigIntEven


  logical function BigIntGE(bi1,bi2)
    type(bigint), intent(in) :: bi1, bi2
    BigIntGE = .not. BigIntLessThan(bi1,bi2)
  end function BigIntGE

  logical function BigIntLE(bi1,bi2)
    type(bigint), intent(in) :: bi1, bi2
    BigIntLE = .not. BigIntGreaterThan(bi1,bi2)
  end function BigIntLE

  logical function BigIntLessThan(bi1,bi2)
    type(bigint), intent(in) :: bi1, bi2
    BigIntLessThan = BigIntGreaterThan(bi2,bi1)
  end function BigIntLessThan

!--------------------------------------------------------------------
!                       GREATER THAN
!--------------------------------------------------------------------

  logical function BigIntGreaterThan(bi1,bi2)
    type(bigint), intent(in) :: bi1, bi2
    integer(kind=intkind) :: i

    if (.not. bi1%allocated) then
       STOP 'BigIntGreaterThan: b1 not allocated'
    end if

    if (.not. bi2%allocated) then
       STOP 'BigIntGreaterThan: b2 not allocated'
    end if 

    if (bi1%digits .gt. bi2%digits) then
       BigIntGreaterThan =.true.
       return
    else if (bi1%digits .lt. bi2%digits) then
       BigIntGreaterThan =.false.
       return
    end if

    !same number of digits then
    !Compare them until we find one smaller..
    do i=bi1%digits,1,-1
       if (bi1%vals(i) .gt. bi2%vals(i)) then
          BigIntGreaterThan =.true.
          return
       else if (bi1%vals(i) .lt. bi2%vals(i)) then
          BigIntGreaterThan =.false.
         return
       end if

    end do
    BigIntGreaterThan =.false.

  end function BigIntGreaterThan


!--------------------------------------------------------------------
!                          EQUALS
!--------------------------------------------------------------------

  logical function BigIntEquals(bi1, bi2)
    type(bigint), intent(in) :: bi1, bi2
    integer(kind=intkind) :: i

    BigIntEquals = .true.

    if (bi1%digits .ne. bi2%digits)  then
       BigIntEquals = .false. 
       return
    end if

    do i=1,bi1%digits
       BigIntEquals = BigIntEquals .and. (bi1%vals(i) .eq. bi2%vals(i))
    end do
    
  end function BigIntEquals


  logical function BigIntEqualsInt(b,int)
    type(bigint), intent(in) :: b
    integer(kind=intkind), intent(in) :: int

    if (int .ge. base) STOP 'int too big in equals...'
    if (int .lt. 0_intkind) STOP 'not implimented for negatives yet'


    if (b%digits .gt. 1) then
       BigIntEqualsInt = .false.
       return
    end if

    BigIntEqualsInt = b%vals(1) .eq. int


  end function BigIntEqualsInt



end module bigintops
