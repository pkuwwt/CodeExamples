!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      array_linear_alg.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Common linear algebra computations.@EOL
!  @Keywords  none
!  @Std       F95
!
!             See the array_sections for some common matrix manipulations.
!             

PROGRAM array_linear_alg

  IMPLICIT NONE

  ! Array initialization (assignments work outside of declarations too)
  REAL, DIMENSION(3, 3) :: a = RESHAPE((/  1,  2,  3,  4,  5,  6,  7,  8,  9 /),             (/ 3, 3 /))
  REAL, DIMENSION(3, 3) :: ap
  REAL, DIMENSION(3, 4) :: b = RESHAPE((/ 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21 /), (/ 3, 4 /))
  REAL, DIMENSION(3, 4) :: bp
  REAL, DIMENSION(3)    :: x =         (/ 22, 23, 24/)
  REAL, DIMENSION(4)    :: y =         (/ 25, 26, 27, 28/)

  PRINT *
  PRINT '(a,9f7.1)',  '               a=', a
  PRINT '(a,3f7.1)',  '               a=', a(1,:)
  PRINT '(a,3f7.1)',  '                 ', a(2,:)
  PRINT '(a,3f7.1)',  '                 ', a(3,:)
  PRINT '(a,12f7.1)', '               b=', b
  PRINT '(a,4f7.1)',  '               b=', b(1,:)
  PRINT '(a,4f7.1)',  '                 ', b(2,:)
  PRINT '(a,4f7.1)',  '                 ', b(3,:)
  PRINT '(a,3f7.1)',  '               x=', x
  PRINT '(a,4f7.1)',  '               y=', y

  PRINT *
  PRINT '(a,3f7.1)',  '             2*x=', 2*x
  PRINT '(a,3f7.1)',  '             x+x=', x+x
  PRINT '(a,9f7.1)',  '             2*a=', 2*a

  PRINT *
  ap=TRANSPOSE(a)
  PRINT '(a,9f7.1)',  '    TRANSPOSE(a)=', ap
  PRINT '(a,3f7.1)',  '    TRANSPOSE(a)=', ap(1,:)
  PRINT '(a,3f7.1)',  '                 ', ap(2,:)
  PRINT '(a,3f7.1)',  '                 ', ap(3,:)

  PRINT *
  PRINT '(a,f7.1)',   'DOT_PRODUCT(y,y)=', DOT_PRODUCT(y,y)
  PRINT '(a,3f7.1)',  '     MATMUL(a,x)=', MATMUL(a,x)
  PRINT '(a,3f7.1)',  '     MATMUL(b,y)=', MATMUL(b,y)

  PRINT *
  bp=MATMUL(a,b)
  PRINT '(a,12f7.1)', '     MATMUL(a,b)=', bp
  PRINT '(a,4f7.1)',  '     MATMUL(a,b)=', bp(1,:)
  PRINT '(a,4f7.1)',  '                 ', bp(2,:)
  PRINT '(a,4f7.1)',  '                 ', bp(3,:)

  PRINT *
  PRINT '(a,3f7.1)',  '  a(1,:) . a(:,1)=', DOT_PRODUCT(a(1,:), a(:,1))
  ap=MATMUL(a, a)
  PRINT '(a,3f7.1)',  '       (a*a)(1,1)=', ap(1,1)

  PRINT *
  ap=a
  ap(2,:)=-2*ap(1,:)+ap(2,:) ! Rank 1 update
  PRINT '(a,9f7.1)',  'a: R2 <- -2*R1+R2=', ap
  PRINT '(a,3f7.1)',  'a: R2 <- -2*R1+R2=', ap(1,:)
  PRINT '(a,3f7.1)',  '                  ', ap(2,:)
  PRINT '(a,3f7.1)',  '                  ', ap(3,:)

END PROGRAM array_linear_alg
