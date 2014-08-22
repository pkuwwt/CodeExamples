!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      array_init2003.f03
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Fortran 2003 array litteral square bracket notation.@EOL
!  @Keywords  none
!  @Std       F2003

PROGRAM array_init2003

  IMPLICIT NONE

  REAL,    DIMENSION(5) :: x = [ 1.2, 2.3, 3.4, 4.5, 5.6 ]        ! floating point list
  INTEGER               :: j                                      ! Declared for the implicit loops next
  REAL,    DIMENSION(8) :: v = [ (1.0*j,j=1,4), (2.0*j,j=1,4) ]   ! Implicit loop
  LOGICAL, DIMENSION(3) :: m = [ .true.,  .true.,  .false. ]      ! Logical vector

  PRINT *, 'x=', x
  PRINT *, 'v=', v
  PRINT *, 'm=', m

END PROGRAM array_init2003
