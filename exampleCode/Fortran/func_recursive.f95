!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      func_recursive.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Factorial computation via direct recursion with a function. @EOL
!  @Keywords  Factorial result direct recursion fortran f95 function
!  @Std       F95
!
!             This program also demonstrates the RESULT keyword for function signatures
!             

PROGRAM func_recursive
  IMPLICIT NONE

  INTEGER :: i

  DO i=0,10
     PRINT '(a,i2,a,i15)', 'ifact(', i, ')=', ifact(i)
  END DO

CONTAINS

  RECURSIVE FUNCTION ifact(n) RESULT (ifactr)  ! You must use RESULT for direct recursion (silly, but true)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    INTEGER             :: ifactr
    IF ( n <=1 ) THEN
       ifactr = 1
    ELSE
       ifactr = n * ifact(n-1)
    END IF
  END FUNCTION ifact

END PROGRAM func_recursive
