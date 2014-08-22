!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      array_reduction.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Array reduction functions COUNT, ALL, and ANY.@EOL
!  @Keywords  none
!  @Std       F95
!
!             COUNT, ALL, ANY are very powerful constructs when combined with element-wise binary operators.  ex: ALL(a>b)
!             

PROGRAM array_reduction

  IMPLICIT NONE

  LOGICAL, DIMENSION(3) :: mixd = (/ .true.,  .true.,  .false. /)          
  LOGICAL, DIMENSION(3) :: allt = (/ .true.,  .true.,  .true.  /)          
  LOGICAL, DIMENSION(3) :: allf = (/ .false., .false., .false. /)          

  REAL, DIMENSION(5)    :: a = (/ 5, 4, 3, 2, 1 /)          

  PRINT *
  PRINT *, 'mixd=', mixd
  PRINT *, 'allt=', allt
  PRINT *, 'allf=', allf

  PRINT *
  PRINT "(A6,A5,A5,A5)",      '', 'mixd',      'allt',      'allf'
  PRINT "(A6,I5,I5,I5)", 'COUNT', COUNT(mixd), COUNT(allt), COUNT(allf)
  PRINT "(A6,L5,L5,L5)", '  ALL', ALL(mixd),   ALL(allt),   ALL(allf)
  PRINT "(A6,L5,L5,L5)", '  ANY', ANY(mixd),   ANY(allt),   ANY(allf)

  PRINT *
  PRINT *, '         a=', a
  PRINT *, '    SUM(a)=', SUM(a)
  PRINT *, 'PRODUCT(a)=', PRODUCT(a)
  PRINT *, ' MAXVAL(a)=', MAXVAL(a)
  PRINT *, ' MINVAL(a)=', MINVAL(a)
  PRINT *, ' MAXLOC(a)=', MAXLOC(a)
  PRINT *, ' MINLOC(a)=', MINLOC(a)


END PROGRAM array_reduction
