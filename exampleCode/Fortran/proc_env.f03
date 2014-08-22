!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      proc_env.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     How to access environment variables. @EOL
!  @Std       F2003

PROGRAM proc_env

  IMPLICIT NONE

  INTEGER, PARAMETER  :: SLEN = 128
  INTEGER             :: ierror, length
  CHARACTER(LEN=SLEN) :: argument
  
  CALL GET_ENVIRONMENT_VARIABLE('PATH', argument, length, ierror, .FALSE.)
  IF(ierror <= 0) THEN
     WRITE (*,*) 'PATH:           ', argument(1:min(SLEN, length))
     IF(ierror < 0) THEN
        WRITE (*,*) '   PATH Length: ', length, ' ---- TOO LONG FOR VARIABLE!'
     ELSE 
        WRITE (*,*) '   PATH Length: ', length
     END IF
  ELSE
     WRITE (*,*) 'PATH could not be retrieved'
  END IF

END PROGRAM proc_env
