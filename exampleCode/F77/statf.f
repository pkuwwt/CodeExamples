CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/statf.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates statement functions in Fortran 77.@EOL
C  @Keywords  statement function FORTRAN f77
C  @Std       F77
C            

       program statf

C      The one line STATEMENT function.  They are local to the unit
C      they are defined in.  They must be one line.  You declare them
C      just as you do variables.

       real avg4
       real a, b, c, d

       AVG4(a, b, c, d) = (a+b+c+d)/4
       write (*,*) 'avg(1,2,3,4)=', AVG4(1.0,2.0,3.0,4.0)

       end
