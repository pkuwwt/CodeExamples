CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/implml.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     hello @EOL
C  @Keywords  FORTRAN f77 do
C  @Std       F77
C
C             Illustrates the IMPLICIT NONE extension to FORTRAN 77
C             that was specified by MIL-STD-1753 (A Department of
C             Defense standard issued in 1988).  The extensions were
C             REQUIRED for a complier to be used for a DoD project, and
C             today they are required for most government contract work
C             with FORTRAN 77.  This extension was already supported by
C             many compilers before MIL-STD-1753, and it is supported
C             by almost all modern FORTRAN 77 compilers.
C
C             This statement is part of the Fortran 90 standard.
C
C             Note that many compilers have a compile time option that
C             will provide warnings for implicit variable declarations
C             regardless of the use of IMPLICIT NONE (with g77 use
C             -Wimplicit)

C     IMPLicit MiL standard
      program implml

C     Making implicit variables illegal is as simple as placing the
C     following before any executable code in every unit unit (main
C     program, subroutine, function, etc...)
      implicit none

C     Declare and use a variable i.
      integer i

C     If uncommented, the following line would be illegal:
C      j = 1

C     The following is fine as i has been declared..
      i = 1

      end
