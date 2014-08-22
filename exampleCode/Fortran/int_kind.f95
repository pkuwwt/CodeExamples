!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      int_kind.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Typical ways to select integer KINDs.@EOL
!  @Keywords  none
!  @Std       F95
!             

PROGRAM int_kind
  IMPLICIT NONE
                                                         ! | typical 32-bit C | C99     | bytes |    N=largest number | log_10(N) |
                                                         ! |------------------+---------+-------+---------------------+-----------|
  INTEGER, PARAMETER :: int8  = SELECTED_REAL_KIND(R=4)  ! | signed char      | int8_t  |     1 |                 127 |  4.844187 |
  INTEGER, PARAMETER :: int16 = SELECTED_REAL_KIND(R=10) ! | signed short     | int16_t |     2 |               32767 | 10.397177 |
  INTEGER, PARAMETER :: int32 = SELECTED_REAL_KIND(R=21) ! | signed int       | int32_t |     4 |          2147483647 | 21.487562 |
  INTEGER, PARAMETER :: int64 = SELECTED_REAL_KIND(R=43) ! | signed long      | int64_t |     8 | 9223372036854775807 | 43.668272 |

  integer(KIND=int8)  :: i8
  integer(KIND=int16) :: i16
  integer(KIND=int32) :: i32
  integer(KIND=int64) :: i64

  WRITE (*,*) 'KIND=int1'
  WRITE (*,*) '  Number of significant digits', DIGITS(i8)      
  WRITE (*,*) '  Largest number              ', HUGE(i8)        
  WRITE (*,*) '  Base of the model           ', RADIX(i8)       
  WRITE (*,*) '  Decimal exponent range      ', RANGE(i8)       

  WRITE (*,*) 'KIND=int2'
  WRITE (*,*) '  Number of significant digits', DIGITS(i16)         
  WRITE (*,*) '  Largest number              ', HUGE(i16)       
  WRITE (*,*) '  Base of the model           ', RADIX(i16)      
  WRITE (*,*) '  Decimal exponent range      ', RANGE(i16)      

  WRITE (*,*) 'KIND=int4'
  WRITE (*,*) '  Number of significant digits', DIGITS(i32)         
  WRITE (*,*) '  Largest number              ', HUGE(i32)       
  WRITE (*,*) '  Base of the model           ', RADIX(i32)      
  WRITE (*,*) '  Decimal exponent range      ', RANGE(i32)      

  WRITE (*,*) 'KIND=int8'
  WRITE (*,*) '  Number of significant digits', DIGITS(i64)         
  WRITE (*,*) '  Largest number              ', HUGE(i64)       
  WRITE (*,*) '  Base of the model           ', RADIX(i64)      
  WRITE (*,*) '  Decimal exponent range      ', RANGE(i64)      

END PROGRAM int_kind









