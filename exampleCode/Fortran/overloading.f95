!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      overloading.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Function overloading fortran style.@EOL
!  @Std       F95
!
!             Unlike C++ overloading, the Fortran method of specifying overloaded functions by name allows for external
!             linkage to libraries that already exist -- like the BLAS.

PROGRAM overloading

  IMPLICIT NONE

  INTERFACE foobar
     FUNCTION foobar_real(X)
       REAL, INTENT(IN) :: x
       REAL             :: foobar_real
     END FUNCTION foobar_real
     FUNCTION foobar_int(X)
       INTEGER, INTENT(IN) :: x
       INTEGER             :: foobar_int
     END FUNCTION foobar_int
  END INTERFACE foobar

  INTEGER  :: int_var  = 2
  REAL     :: real_var = 2.0

  PRINT '(a,i10)',   'function=', foobar_int(int_var)
  PRINT '(a,f10.3)', 'function=', foobar_real(real_var)
  PRINT '(a,i10)',   'function=', foobar(4)
  PRINT '(a,f10.3)', 'function=', foobar(4.0)

END PROGRAM overloading

FUNCTION foobar_int(x)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: x
  INTEGER             :: foobar_int
  foobar_int = x * x
  RETURN
END FUNCTION foobar_int

FUNCTION foobar_real(x)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x
  REAL             :: foobar_real
  foobar_real = x * x * x
  RETURN
END FUNCTION foobar_real
