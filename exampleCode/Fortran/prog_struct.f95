!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      prog_struct.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     The typical structure of a program file. @EOL
!  @Std       F95

PROGRAM prog_struct
  USE mod_struct

  IMPLICIT NONE

  INTEGER  :: i = 2

  ! INTERFACE required because outside_func is not in the CONTAINS section
  INTERFACE
     FUNCTION outside_func(X)
       INTEGER, INTENT(IN) :: x
       INTEGER             :: outside_func
     END FUNCTION outside_func
  END INTERFACE

  ! You don't need empty () to call a subroutine with no args...
  CALL a_sub_no_args

  WRITE (*,*) 'i=', i
  ! Note the second arg must be a variable that can be set in this case...
  CALL a_sub_with_args(1, i)
  WRITE (*,*) 'i=', i

  WRITE (*,*) 'osf=', outside_func(i)

  CALL hi_ma

CONTAINS

  SUBROUTINE a_sub_no_args
    IMPLICIT NONE
    ! variable and routine declarations go here
    WRITE (*,*) 'Hello, World!'
    ! return is not required at the end..
    RETURN
  END SUBROUTINE a_sub_no_args

  SUBROUTINE a_sub_with_args(an_in_arg, an_out_arg)
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: an_in_arg
    INTEGER, INTENT(OUT) :: an_out_arg
    an_out_arg = an_in_arg ! This will change the an_out_arg var in caller!!
    ! return is not required at the end..
    RETURN
  END SUBROUTINE a_sub_with_args

  FUNCTION a_func(an_arg, another_arg)
    IMPLICIT NONE
    ! Note: INTENT(IN) on all args -- God fearing functions never modify args!
    INTEGER, INTENT(IN) :: an_arg        
    INTEGER, INTENT(IN) :: another_arg
    ! Must declare return type
    INTEGER             :: a_func
    ! This sets the return value
    a_func = an_arg + another_arg
    ! return is not required at the end..
    RETURN
  END FUNCTION a_func

END PROGRAM prog_struct

FUNCTION outside_func(x)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: x
  INTEGER             :: outside_func
  outside_func = x * x
  RETURN
END FUNCTION outside_func
