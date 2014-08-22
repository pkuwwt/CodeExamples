!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  @file      real_kinds.f95
!  @Author    Mitch Richling<http://www.mitchr.me>
!  @Copyright Copyright 2012 by Mitch Richling.  All rights reserved.
!  @brief     Typical way to declare single and double precision real variables. @EOL
!  @Std       F95

PROGRAM real_kinds
  IMPLICIT NONE

  ! Standard ways to get reals:
  INTEGER, PARAMETER :: REK1 = KIND(0.0E0) 
  REAL(KIND=REK1)    :: re1
  REAL               :: re2  ! Same type as re1

  ! Standard ways to get real doubles:
  INTEGER, PARAMETER :: dpk1 = KIND(1.0D0)
  INTEGER, PARAMETER :: dpk2 = selected_real_kind(2*precision(1.0_REK1))
  REAL(KIND=dpk1)    :: dp1
  REAL(KIND=dpk2)    :: dp2
  DOUBLE PRECISION   :: dp3 ! Same type as dp1

  WRITE (*,*) 'Kind re1=', KIND(re1)
  WRITE (*,*) 'Kind re2=', KIND(re2)

  WRITE (*,*)
  WRITE (*,*) 'Kind dp1=', KIND(dp1)
  WRITE (*,*) 'Kind dp2=', KIND(dp2)
  WRITE (*,*) 'Kind dp3=', KIND(dp3)

  WRITE (*,*)
  WRITE (*,*) 'Real (re1) Info'
  WRITE (*,*) '   Number of significant digits       ', DIGITS(re1)         
  WRITE (*,*) '   Almost negligible compared to one  ', EPSILON(re1)    
  WRITE (*,*) '   Largest number                     ', HUGE(re1)       
  WRITE (*,*) '   Maximum model exponent             ', MAXEXPONENT(re1) 
  WRITE (*,*) '   Minimum model exponent             ', MINEXPONENT(re1) 
  WRITE (*,*) '   Decimal precision                  ', PRECISION(re1)  
  WRITE (*,*) '   Base of the model                  ', RADIX(re1)      
  WRITE (*,*) '   Decimal exponent range             ', RANGE(re1)      
  WRITE (*,*) '   Smallest positive number           ', TINY(re1)       

  WRITE (*,*)
  WRITE (*,*) 'Double (dp1) Info'
  WRITE (*,*) '   Number of significant digits       ', DIGITS(dp1)         
  WRITE (*,*) '   Almost negligible compared to one  ', EPSILON(dp1)    
  WRITE (*,*) '   Largest number                     ', HUGE(dp1)       
  WRITE (*,*) '   Maximum model exponent             ', MAXEXPONENT(dp1) 
  WRITE (*,*) '   Minimum model exponent             ', MINEXPONENT(dp1) 
  WRITE (*,*) '   Decimal precision                  ', PRECISION(dp1)  
  WRITE (*,*) '   Base of the model                  ', RADIX(dp1)      
  WRITE (*,*) '   Decimal exponent range             ', RANGE(dp1)      
  WRITE (*,*) '   Smallest positive number           ', TINY(dp1)       

END PROGRAM real_kinds
