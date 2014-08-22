CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/intfil.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates the use of internal files in Fortran 77.@EOL
C  @Keywords  formated i/o internal file FORTRAN 77
C  @Std       F77
C

       program intfil

       character chrstr*10
       real      rnum

C     A rewind is done each time on single (non-array) variables
      write(chrstr, fmt='(f7.2)') 12.34
      write (*,*) 'After first write:  ', chrstr
C     You don't have to have a format specified
      write(chrstr,*) 12.34
      write (*,*) 'After second write: ', chrstr

C     Now we convert a string into a real
      read(chrstr, *) rnum
      write (*,*) 'The value converted was: ', rnum

      end

C     Note that the target/source of an internal read/write can be a
C     character type, a "substring" of a character type, or an element
C     of a character array.
