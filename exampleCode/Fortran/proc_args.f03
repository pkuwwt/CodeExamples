!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      proc_args.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     How to access command line arguments. @EOL
!  @Std       F2003

PROGRAM proc_args

  IMPLICIT NONE

  INTEGER, PARAMETER  :: SLEN = 20
  INTEGER             :: i, ierror, length
  CHARACTER(LEN=SLEN) :: argument
  
  CALL GET_COMMAND(argument, length, ierror)
  IF(ierror <= 0) THEN
     WRITE (*,*) 'Entire command line: ', argument(1:min(SLEN, length))
     IF(ierror < 0) THEN
        WRITE (*,*) '   Length was:      ', length, ' ---- TOO LONG FOR VARIABLE!'
     ELSE 
        WRITE (*,*) '   Length was:      ', length
    END IF
  ELSE
     WRITE (*,*) 'Command line could not be retrieved'
  END IF

  DO i = 1,COMMAND_ARGUMENT_COUNT()
     CALL GET_COMMAND_ARGUMENT(i, argument, length, ierror)
     IF(ierror <= 0) THEN
        WRITE (*,*) 'Argument:      ', argument(1:min(SLEN, length))
        WRITE (*,*) '   Arg Number: ', i
        IF(ierror < 0) THEN
           WRITE (*,*) '   Length was: ', length, ' ---- TOO LONG FOR VARIABLE!'
        ELSE 
           WRITE (*,*) '   Length was: ', length
        END IF
     ELSE
        WRITE (*,*) 'Argument ', i, 'could not be retrieved'
     END IF
  END DO

END PROGRAM proc_args
