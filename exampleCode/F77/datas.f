CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/datas.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates the FORTRAN data statement.@EOL
C  @Keywords  data statement FORTRAN f77
C  @Std       F77
C            

      program datas

C     Declaration optional (a and b are real by default)
      real a, b, c(3), d(3), e(2,2), f

C     Put f in a common block...
      common /foo/f

C     Initialize simple variables like so:
      data a/10/, b/1.23/

C     Initialize single elements of an array, but it is a sorta
C     creepy thing to do:
      data c(1)/1/, c(3)/3/

C     You can init an entire vector like this:
      data d/1,2,3/

C     You can init multi-dimensional arrays in the same way, but
C     remember FORTRAN stores things in column major order:
      data e/1,2,3,4/

C     The following don't always work, so they are commented out..
C     You can initialize multiple variables to the SAME value:
C       data g,h/2*10/
C     You can often initialize an array (to 0 in this case) like this:
C       data c/3*0/
C     You can initialize several variables like this too:
C       data a,b/10,1.23/

      write (*,*) 'a=', a
      write (*,*) 'b=', b
      write (*,*) 'c(1)=', c(1), ', c(2)=', c(2), ', c(3)=', c(3)
      write (*,*) 'd(1)=', d(1), ', d(2)=', d(2), ', d(3)=', d(3)
      write (*,*) 'e(1,1)=', e(1,1), ', e(1,2)=', e(1,2), 
     *          ', e(2,1)=', e(2,1),', e(2,2)=', e(2,2)
      write (*,*) 'f=', f

      end


C     You can not use a data statement for stuff contained
C     in a common block.  For this, you must use the 'block data'
C     construct -- treated very much the same way as a subroutine.
      block data
       real f
       common /foo/f
       data f/10/
      end
