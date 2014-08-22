CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/loopml.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     do-loop extension specified in MIL-STD-1753.@EOL
C  @Keywords  FORTRAN f77 do
C  @Std       F77
C
C             Illustrates a common do-loop extension to FORTRAN 77 that
C             was specified by the MIL-STD-1753 (A Department of
C             Defense standard issued in 1988).  The extensions were
C             REQUIRED for a compiler to be used for a DoD project, and
C             today they are required for most government contract work
C             with FORTRAN 77.  The "end do" syntax is a logical
C             extension given the 'end if' syntax in standard FORTRAN
C             77 -- really this should have been in the standard!  This
C             extension was already supported by many compilers before
C             MIL-STD-1753, and it is supported by just about every
C             compiler today.

C     loop MiL standard
      program loopml

      integer i

C     The MIL-STD-1753 do-loop syntax (no continue or line number!):
      do i=1,10
        write (*,*) 'i=', i
      end do

      end
