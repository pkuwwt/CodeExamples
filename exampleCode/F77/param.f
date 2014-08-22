CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      param.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates the FORTRAN parameter statement.@EOL
C  @Keywords  parameter statement FORTRAN f77
C  @Std       F77
C            

      program param 

C     Parameters are named constants with usage rules much like
C     that found in C macros -- except parameters have real
C     scoping rules.  Note that they are typed, just like regular
C     variables.  You can use parameters in places you can not
C     use variables, but one parameter may NOT depend upon another.
      real a, b, i
      parameter (a=1, b=3.2, i=5.4)

      write (*,*) 'a=', a
      write (*,*) 'b=', b
      write (*,*) 'i=', i

      end
