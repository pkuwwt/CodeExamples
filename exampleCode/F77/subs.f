CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/subs.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates subroutines in Fortran 77.@EOL
C  @Keywords  subroutine FORTRAN f77
C  @Std       F77
C            

       program subs

       integer j, k
       real am

       dimension am(10)
       do 100 j=1,10
          am(j) = j
 100   continue
       k=0
       write (*,*) 'Calling sub1... k=', k
       call sub1(k)
       write (*,*) 'Back in main.. k=', k
       write (*,*) 'Calling sub2...'
       call sub2(am)

       end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      Sub number 1.  Note the return statement
C      Note that parameters are passed by REFERENCE
       subroutine sub1(i)
       integer i
       write (*,*) 'In sub1... i=', i
       i=3
       write (*,*) 'Still in sub1... i=', i
       return
       end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      This one calls another sub.
C      Note you have to provide a dimension for ar inside the 
C      subroutine via  declaration or dimension statement.
       subroutine sub2(ar)
       real ar(10)
       write (*,*) 'In sub2... ar=', ar
       write (*,*) 'Calling sub3 from sub2'
       call sub3
       write (*,*) 'Still in sub2'
       return
       end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      This sub is called by a sub
       subroutine sub3
       write (*,*) 'In sub3'
       return
       end

