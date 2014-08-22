!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      func_opt_arg.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Optional function arguments. @EOL
!  @Std       F95
!
!             Optional arguments work the same for subroutines

PROGRAM func_opt_arg

  IMPLICIT NONE
  PRINT '(a,i3)', 'a_func(1, 2)                 =', a_func(1, 2)
  PRINT '(a,i3)', 'a_func(1, 2, 3)              =', a_func(1, 2, 3)
  PRINT '(a,i3)', 'a_func(1, 2, arg3=3)         =', a_func(1, 2, arg3=3)
  PRINT '(a,i3)', 'a_func(1, 2, 3,      4)      =', a_func(1, 2, 3,      4)
  PRINT '(a,i3)', 'a_func(1, 2, arg3=3, arg4=4) =', a_func(1, 2, arg3=3, arg4=4)
  PRINT '(a,i3)', 'a_func(1, 2,         arg4=4) =', a_func(1, 2,         arg4=4)

CONTAINS

  FUNCTION a_func(arg1, arg2, arg3, arg4)
    IMPLICIT NONE
    INTEGER, INTENT(IN)           :: arg1, arg2
    INTEGER, INTENT(IN), OPTIONAL :: arg3, arg4
    INTEGER                       :: a_func
    a_func = arg1 + 2*arg2
    if(present(arg3)) a_func = a_func + 3*arg3
    if(present(arg4)) a_func = a_func + 4*arg4
    RETURN
  END FUNCTION a_func

END PROGRAM func_opt_arg
