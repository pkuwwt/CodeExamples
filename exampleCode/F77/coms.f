CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/coms.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates the FORTRAN common block.@EOL
C  @Keywords  common block fortran
C  @Std       F77
C

       program coms

C      Declaration optional (a and b are real by default)
       real a, b

C      COMMON let's us specify that vars will
C      be common to all code blocks that request it
       common a

C      You can name a common block too.  Consider this
       common /foo/b

       a=1.1
       b=2.2
       write (*,*) 'In main... a=', a
       write (*,*) 'In main... b=', b
       write (*,*) 'In main... Calling sub1'
       call sub1
       write (*,*) 'In main... a=', a
       write (*,*) 'In main... b=', b
       call sub2
       write (*,*) 'In main... a=', a
       write (*,*) 'In main... b=', b
       write (*,*) 'Back in main..'

       end

C      Sub number 1.  
       subroutine sub1
       real a, b
       common a
       write (*,*) 'In sub1... a=', a
       write (*,*) 'In sub1... b has no value here'
       a=10
       b=20
       write (*,*) 'In sub1... a=', a
       write (*,*) 'In sub1... b=', b
       return
       end

C      Sub number 2.
       subroutine sub2
       real a, b
       common /foo/b
       write (*,*) 'In sub2... a has no value here'
       write (*,*) 'In sub2... b=', b
       a=30
       b=40
       write (*,*) 'In sub1... a=', a
       write (*,*) 'In sub1... b=', b
       return
       end
