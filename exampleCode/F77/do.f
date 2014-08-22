CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      do.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates several common do-loop constructs.@EOL
C  @Keywords  FORTRAN f77 do
C  @Std       F77
C            

       program do

C      Optional declarations
       integer I
       real    X

C      Traditional do-loop
       do 100 I=1,3
          write (*,*) 'I=', I
 100   continue

C      do-loop with an increment.  This one happens to have a 
C      REAL loop variable -- not allowed in FORTRAN 95.
       do 200 X=1,2,0.3
          write (*,*) 'X=', X
 200   continue

C      Fortran 90 and beyond allow 'end do' to terminate a loop 
C      with no line number required!  Many Fortran 77 compilers,
C      including g77, allow this too.
       do I=1,3
          write (*,*) 'I=', I
       end do

C      The typical idiom for 'break' in FORTRAN 77.
C      This will typically print a warning...
       do 300 I=1,10
          write (*,*) 'I=', I
          if (I .gt. 3) goto 350
 300   continue
 350   continue

C      The typical idiom for 'next' in FORTRAN 77.
       do 400 I=1,7 
          if (I .le. 5) goto 400
          write (*,*) 'I=', I
 400   continue

       end
