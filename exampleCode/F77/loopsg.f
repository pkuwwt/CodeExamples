CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/loopsg.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @Revision  $Revision$ 
C  @SCMdate   $Date$
C  @brief     goto replacements.@EOL
C  @Keywords  FORTRAN f77 do
C  @Std       F77 MIL-STD-1753
C
C             Illustrates two common goto replacements for repeat-until
C             (do-while) and while-do type loops.  This kind of thing
C             is still commonly found in working code.

C     LOOPS with Goto
      program loopsg

      real X

      write (*,*) 'Typical "while-do" loop idiom.'
      X = 5
100   if (X .ge. 2) then
        write (*,*) 'X=', X
        X = X - 1
        goto 100
      endif 

       write (*,*) 'This is a typical "do-while" construct.'
       X=5
 400   continue
       X = X - 1
       write (*,*) 'X=', X
       if (X .ge. 3) goto 400

       write (*,*) 'This is a typical "repeat-until" construct.'
       X=5
 500   continue
       X = X - 1
       write (*,*) 'X=', X
       if (X .le. 0) goto 550
       goto 500
 550   continue

       write (*,*) 'This is another typical "while-do" idiom'
       x=5
 600   if (X .le. 0) goto 650
       write (*,*) 'X=', X
       X = X - 1
       goto 600
 650   continue

       end
