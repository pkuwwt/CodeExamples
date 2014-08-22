CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      savey.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates save statement in Fortran 77.@EOL
C  @Keywords  save statement FORTRAN f77
C  @Std       F77 
C

       program savey

       integer i
       integer memfun

       do 10 i=1,10
        write (*,*) 'memfun()... =', memfun()
10     continue
       write (*,*) "DONE"

       end

C      This function illustrates the use of the save statement
       integer function memfun() 
C       Declare our "saved" variable
        integer lastv
C       Tell the compiler to save this value upon exit
        save lastv
C       Set lastv to '0' the FIRST TIME the function runs.
        data lastv/0/
C       The result of the function as normal.. 
        lastv = lastv + 1
        memfun=lastv
        return
       end

