CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      goto.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     An infinite loop illustrated with fortran's goto.@EOL
C  @Keywords  goto FORTRAN f77
C  @Std       F77
C

        program gotot

C       The GOTO...
C       This program is an infinite loop!

 100    continue
        write (*,*) 'hello....'
        goto 100
        
       end
