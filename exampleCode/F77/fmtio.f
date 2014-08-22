CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/fmtio.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates formated output in Fortran 77. @EOL
C  @Keywords  formated i/o screen stdin stdout FORTRAN 77
C  @Std       F77
C            

       program fmtio

       character chs*7

       write (*,FMT='(1X,F6.2)') 1234.56
       write (*,FMT='(1X,F7.2)') 1234.56
       write (*,FMT='(1x,F8.2)') 1234.56

       write (*,FMT='(1x,I6.6)') 1234
       write (*,FMT='(1x,I6.5)') 1234
       write (*,FMT='(1x,I6)')   1234

C      'T' is handy to line things up when stuff up when you don't
C      know how wide the stuff t the left is..
       write (*,FMT='(1x,I5,T20,I6.6)') 1234,   1234
       write (*,FMT='(1x,I6,T20,I6.6)') 123456, 1234

C      'TR' is NOT the same as 'T'.  It simply shifts:
       write (*,FMT='(1x,I5,TR20,I6.6)') 1234,   1234
       write (*,FMT='(1x,I6,TR20,I6.6)') 123456, 1234

C      'nX' is the same as 'TRn' and is more common in older code:
       write (*,FMT='(1x,I5,20X,I6.6)') 1234,   1234
       write (*,FMT='(1x,I6,20X,I6.6)') 123456, 1234

C      You can use a string as a format statement.
       chs = '(1x,I6)'
       write(*,FMT=chs) 123



C Integer                             rIw, rIw.m
C real, double precision, or comlex   rEw.d, rEw.dEe, rFw.d, rGw.d, rGw.dEe
C Logicla                             rLw
C Character                           rA, rAw
C
C w is the total field width.
C m is the minimum number of digits produced on output.
C d is the number of digits after the decimal point.
C e is the number of digits used for the exponent.
C r is a repeat (handy for arrays)
C Tn -- move to column n
C TRn -- Shift to the right by n columns
C TLn -- Shift to the left by n columns
C nX  -- Same as TRn
C SP  -- after all positive numbers have a +
C SS  --  after the + is suppresed


       end
