!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      case_statement.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     The case statement. @EOL
!  @Std       F95

program case_statement
  IMPLICIT NONE

  INTEGER :: i

  SELECT CASE(.TRUE.)
  CASE(.TRUE.)
     WRITE (*,*) "LOGICAL CASE: .TRUE."
  CASE(.FALSE.)
     WRITE (*,*) "LOGICAL CASE: .FALSE."
  END SELECT

  SELECT CASE('AB')
  CASE('A')
     WRITE (*,*) "CHAR CASE: A      "
  CASE('B', 'C')
     WRITE (*,*) "CHAR CASE: B,C    "
  CASE('E':'K')
     WRITE (*,*) "CHAR CASE: E:K    "
  CASE('AB')
     WRITE (*,*) "CHAR CASE: AB     "
  CASE DEFAULT
     WRITE (*,*) "CHAR CASE: DEFAULT"
  END SELECT

  DO i=0,11
     SELECT CASE(i)
     CASE(1)
        WRITE (*,*) "INT CASE: 1      ", i
     CASE(2,4)
        WRITE (*,*) "INT CASE: 2,4    ", i
     CASE(5:10)
        WRITE (*,*) "INT CASE: 5:10   ", i
     CASE DEFAULT
        WRITE (*,*) "INT CASE: DEFAULT", i
     END SELECT
  END DO

END PROGRAM case_statement
