!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      array_init1995.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Array assignment and initialization.@EOL
!  @Keywords  none
!  @Std       F95

PROGRAM array_init1995

  IMPLICIT NONE

  ! Array initialization (assignments work outside of declarations too)
  REAL,    DIMENSION(5) :: x = (/ 1.2, 2.3, 3.4, 4.5, 5.6 /)          ! floating point list
  REAL,    DIMENSION(5) :: y = (/   5,   4,   3,   2,   1 /)          ! Don't need the dot
  REAL,    DIMENSION(5) :: z = 3                                      ! Set all elements to 3
  INTEGER, DIMENSION(4) :: i = (/   2,   3,   4,   5 /)         
  INTEGER               :: j                                          ! Declared for the implicit loops next
  REAL,    DIMENSION(8) :: v = (/ (1.0*j,j=1,4), (2.0*j,j=1,4) /)     ! Implicit loop
  LOGICAL, DIMENSION(3) :: m = (/ .true.,  .true.,  .false. /)          

  WRITE (*,*) 'INTEGER'
  WRITE (*,*) '  i=          ', i

  WRITE (*,*) 'LOGICAL'
  WRITE (*,*) '  m=          ', m

  WRITE (*,*) 'REAL'
  WRITE (*,*) '  x=          ', x
  WRITE (*,*) '  x(2:4)=     ', x(2:4)
  WRITE (*,*) '  x(1:5:2)=   ', x(1:5:2)
  WRITE (*,*) '  x(i)=       ', x(i)
  WRITE (*,*) '  2*x(1:5:2)= ', 2*x(1:5:2)
  WRITE (*,*) '  x(1:5:2)**2=', x(1:5:2)**2
  WRITE (*,*) '  TAN(x)=     ', TAN(x)
  WRITE (*,*) '  y=          ', y
  WRITE (*,*) '  x+y=        ', x+y
  WRITE (*,*) '  z=          ', z
  WRITE (*,*) '  v=          ', v

  ! PRINT *, '     SHAPE(matrix)=', SHAPE(matrix)
  ! PRINT *, '      SIZE(matrix)=', SIZE(matrix)

  ! PRINT *, '     SHAPE(vector)=', SHAPE(vector)
  ! PRINT *, '      SIZE(vector)=', SIZE(vector)


END PROGRAM array_init1995
