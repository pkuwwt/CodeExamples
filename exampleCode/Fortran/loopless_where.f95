!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      loopless_where.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     The where construct.@EOL
!  @Std       F95
!
!             Statements in WHERE can occur in any order (or even concurrently)

PROGRAM loopless_where
  IMPLICIT NONE

  INTEGER, DIMENSION(5) :: x = (/ 1, 2, 3, 4, 5 /)
  INTEGER, DIMENSION(5) :: y = 0
  INTEGER, DIMENSION(5) :: z = 0

  WRITE (*,*) 'Set x=2*x where x>3'
  WRITE (*,*) '  BEFORE x=   ', x
  WHERE (x .gt. 3) x = x * 2
  WRITE (*,*) '  AFTER  x=   ', x

  WRITE (*,*) 'Set y=x where x>3'
  WRITE (*,*) '  BEFORE y=   ', y
  WHERE (x .gt. 3) y=x
  WRITE (*,*) '  AFTER  y=   ', y

  WRITE (*,*) 'Set z=2*y where x>3'
  WRITE (*,*) '  BEFORE z=   ', z
  WHERE (x .gt. 3)
     z = y * 2
  END WHERE
  WRITE (*,*) '   AFTER z=   ', z

  WRITE (*,*) 'Set z=y where x>3, z=x where x<=3'
  WRITE (*,*) '  BEFORE z=   ', z
  WHERE (x .gt. 3)
     z = y
  ELSEWHERE
     z = x
  END WHERE
  WRITE (*,*) '   AFTER z=   ', z

END PROGRAM loopless_where
