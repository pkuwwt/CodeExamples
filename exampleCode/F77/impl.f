CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      impl.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates the IMPLICIT statement.@EOL
C  @Keywords  FORTRAN f77 do
C  @Std       F77
C
C             Illustrates the IMPLICIT statement -- a rather dubious
C             feature one often encounters in older code.  In my
C             experience most modern programmers don't use this any
C             more -- in fact, many people use the MIL-STD-1753
C             extension IMPLICIT NONE to turn this feature off (see
C             implml.f).

C     IMPLicit statement
      program impl

C     The IMPLICIT statement lets one change the default implicit
C     variable types [ IMPLICIT real (a-h), integer (i-n), real (o-z) ].
      implicit double precision (d,x-z), integer (a-c), complex (e,f)

C     You can still override with an explicit declaration:
      integer x

      a = 10.4
      x = 10.3

      write (*,*) 'a=', a
      write (*,*) 'x=', x


      end
