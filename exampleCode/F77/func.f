CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      func.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates functions in FORTRAN 77.@EOL
C  @Keywords  functions FORTRAN f77
C  @Std       F77
C            

       program func

C      You should declare function return types just like
C      you do variables.  It is not required, but...
       real avg4
       integer iavg4

C      Fortran is real picky about the types of arguments
C      and the return type.  Note the types in the calls.

       write (*,*) 'avg4(1,2,3,4)=',  avg4(1.0,2.0,3.0,4.0)
       write (*,*) 'iavg4(1,2,3,4)=', iavg4(1,2,3,4)

       end


C      This function is real and has four real arguments
C      The types are determined by the names.  Could
C      put REAL in front of the function keyword.
       real function avg4(x1, x2, x3, x4)
       real x1, x2, x3, x4
       avg4 = (x1+x2+x3+x4)/4
       return
       end


C      This function is integer and has four integer arguments
C      The types are determined by the name of the function and
C      the names of the arguments.  Could put INTEGER in front
C      of the function keyword.
       integer function iavg4(i1, i2, i3, i4)
       integer i1, i2, i3, i4
       iavg4 = (i1+i2+i3+i4)/4
       return
       end
