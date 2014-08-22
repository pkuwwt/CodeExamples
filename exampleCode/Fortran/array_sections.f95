!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      array_sections.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Common linear algebra computations.@EOL
!  @Keywords  none
!  @Std       F95
!             

PROGRAM array_sections

  IMPLICIT NONE

  ! We put TARGET on a because we will point a pointer at a subsection later. 
  REAL, DIMENSION(3, 3), TARGET  :: a = RESHAPE(SOURCE=(/  1,  2,  3,  4,  5,  6,  7,  8,  9 /), SHAPE=(/ 3, 3 /))
  REAL, DIMENSION(3)             :: x = (/ 10, 11, 12/)
  REAL, DIMENSION(:, :), POINTER :: ap

  PRINT *
  PRINT '(a,9f7.1)',  '                a=', a
  PRINT '(a,3f7.1)',  '                a=', a(1,:)
  PRINT '(a,3f7.1)',  '                  ', a(2,:)
  PRINT '(a,3f7.1)',  '                  ', a(3,:)
  PRINT '(a,3f7.1)',  '                x=', x
  PRINT *
  PRINT '(a,9f7.1)',  '           a(2,2)=', a(2,2)
  PRINT '(a,9f7.1)',  '         a(2,2:3)=', a(2,2:3)
  PRINT '(a,9f7.1)',  '       a(1:2,2:3)=', a(1:2,2:3)
  PRINT '(a,9f7.1)',  '         a(1:2,2)=', a(1:2,2)
  PRINT '(a,9f7.1)',  '      a(3:1:-1,2)=', a(3:1:-1,2)
  PRINT '(a,9f7.1)',  '       a(1:2:2,2)=', a(1:3:2,2)
  PRINT '(a,9f7.1)',  '    a((/1, 3/),2)=', a((/1, 3/),2)
  PRINT '(a,9f7.1)',  '           a(2,:)=', a(2,:)
  PRINT '(a,3f7.1)',  '           x(2:3)=', x(2:3)
  PRINT '(a,3f7.1)',  '       x((/1,3/))=', x((/1,3/))
  ap => a(2:3, 1:3)
  PRINT '(a,9f7.1)',  '               ap=', ap
  PRINT '(a,3f7.1)',  '               ap=', ap(1,:)
  PRINT '(a,3f7.1)',  '                  ', ap(2,:)

END PROGRAM array_sections
