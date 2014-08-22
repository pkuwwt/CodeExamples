CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      io.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates screen/keyboard I/O in Fortran 77.@EOL
C  @Keywords  i/o screen stdin stdout FORTRAN 77
C  @Std       F77 
C            

        program io

       integer iarray(3)
       real    val1, val2, val3

C      write (*,*) with no arguments prints a blank line
       write (*,*)

C      The (*,*) sends to STDOUT.
       write (*,*) 'Enter two numbers'
       read  (*,*) val1, val2
       write (*,*) 'The numbers: ', val1, val2

C      Simple form of write (*,*) is print *.  Note that this
C      function is deprecated.
       print *, 'input another number'

C      Simple form of read.  Note that this function is deprecated.
       read *, val3
       print *, 'The last number: ', val3 

C      You can read and write arrays too:
       write (*,*) 'Enter three values (for array)'
       read  (*,*) iarray
       write (*,*) 'The array', iarray

       end
