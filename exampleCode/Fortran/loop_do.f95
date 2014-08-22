!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      loop_do.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     The do loop and symbolic lables.@EOL
!  @Std       F95

PROGRAM loop_do

  IMPLICIT NONE
  INTEGER :: i, j

  
  ! end do is finally part of the language.  i MUST be an integer.
  WRITE (*,*) 'Example Loop 1'
  DO i=1,20,2
     WRITE (*,*) '  PRE-CYCLE: ', i
     IF( (I .GT. 2) .AND. (I .LT. 12) ) THEN
        CYCLE
     END IF
     WRITE (*,*) ' POST-CYCLE: ', i
     IF(I .GT. 14) THEN
        EXIT
     END IF
  END DO

  ! Loops can have names, and cycle/exit can use them
  WRITE (*,*) 'Example Loop 2'
  out_loop: DO i=1,3
     in_loop: DO j=1,3
        WRITE (*,*) '  PRE-CYCLE: ', i, j
        IF(I .EQ. 2) THEN
           CYCLE out_loop
        END IF
     WRITE (*,*) ' POST-CYCLE: ', i, j
  END DO in_loop
  END DO out_loop

  ! Do WHILE, and end do are in the standard too!
  ! Like regular do loops, they can have names too.
  WRITE (*,*) 'Example Loop 3'
  i=0
  while_loop: DO WHILE (I .LT. 5)
     i=i+1
     IF(i .EQ. 3) THEN
        CYCLE
     END IF
     WRITE (*,*) ' i=', i
  END DO while_loop

  ! Infinite loops are easy too:
  WRITE (*,*) 'Example Loop 3'
  i=0
  DO
     i=i+1
     IF(I .EQ. 3) THEN
        EXIT
     END IF
     WRITE (*,*) ' i=', i
  END DO

END PROGRAM loop_do
