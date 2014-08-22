CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/arith.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1991 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates some arithmetic concepts in FORTRAN.@EOL
C  @Keywords  FORTRAN f77 arithmetic
C  @Std       F77 MIL-STD-1753 LongNames F90 F95 
C

        program arith

C       Integer arithmetic works in FORTRAN too.
        write (*,*) '(5/2)=                 ', (5/2)

C       As in C, things will be done correctly in this case
        write (*,*) '(5/2.0)=               ', (5/2.0)

C       Fortran has the idea of "pairs" in expressions.  An expression
C       can only be evaluated by evaluating the binary operations one
C       at a time.  So the promotion rules in Fortran ONLY consider such
C       pairs of arguments -- things have a definite order of evaluation
C       in FORTRAN (different from languages like C)
C
C       5/2 is integer (as both 5 and 2 are integers).  
C       Everything is evaluated as integer.
C       5/2.0 is mixed (5 is integer, 2.0 is real).  
C       Everything is promoted to real, and a real result is evaluated.
C       This leads to 2+2.5 which is mixed (2 is integer, and 2.5 is real).
C       Everything is promoted to real, and a real result is evaluated.
        write (*,*) '(5/2+5/2.0)=           ', (5/2+5/2.0)

C      Things go from left to right -- and that is important for integer
C      operations.  () are treated as grouping symbols.  Remember the
C      pair thing for the last example.
       write (*,*) '5/2*2=                  ', 5/2*2
       write (*,*) '5*2/2=                  ', 5*2/2
       write (*,*) '2*(5/2)=                ', 2*(5/2)
       write (*,*) '2.0*(5/2)=              ', 2.0*(5/2)

C      You can make various conversions inside of an expression explicit so
C      that other programmers, perhaps even yourself, will have a better
C      chance of understanding code.  The following examples are silly, but
C      they exhibit the syntax
       write (*,*) 'real(5)/real(2)*real(2)=', real(5)/real(2)*real(2)
       write (*,*) 'real(5)/2*2=            ', real(5)/2*2
       write (*,*) 'real(5/2*2)=            ', real(5/2*2)
       write (*,*) 'int(5/2.2)*5=           ', int(5/2.2)*5

       end
