C  @file      minStdRandGenF.f
C  @Author    Mitch Richling<http://www.mitchr.me/>
C  @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
C  @breif     minimal implementation of the minimal standard random number generator@EOL
C  @Keywords  none
C  @Std       F77 MIL-STD-1753
C  @Notes     See the C version for extensive algorithm notes.

      PROGRAM MNSDTG
      IMPLICIT NONE

      INTEGER i, randN
      INTEGER M, A, Q, R

      M = 2147483647
      A = 16807
      Q = M / A 
      R = MOD(M, A)

      randN = 1

      DO i=1,10
         randN = A*MOD(randN, Q) - R*(randN / Q);
         IF(randN .LE. 0) THEN
            randN = randN + M
         END IF
         WRITE (*,*) randN
      END DO
      END
