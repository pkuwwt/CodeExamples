CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      arrsub.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Arrays in FORTRAN 77.@EOL
C  @Keywords  FORTRAN f77 array
C  @Std       F77 MIL-STD-1753 LongNames F90 F95 
C
C             Illustrates some array concepts as they relate to
C             subprograms in FORTRAN 77

C     ARRays in SUBroutines
      program arrsub

C     We need some arrays:
      real X(5), Y(5)

C     Fill them up:
      data X/1,2,3,4,5/, Y/6,7,8,9,10/

      write (*,*) 'X=', X
      write (*,*) 'Y=', Y

C     After a call to sub1, X is different
      call sub1(X, 5)
      write (*,*) 'X=', X

C     Y is changed by sub2 in a strange way..
      call sub2(Y)
      write (*,*) 'Y=', Y

C     In general, passing an array element passes a POINTER
C     to that array location -- think about it for a moment
C     and this appears reasonable as EVERYTHING is passed
C     as a pointer.  A common idiom is to pass something
C     like Z(1,m) and think of it as "the m-th column of 
C     the matrix Z".  Fortran 90 has explicit subsection
C     syntax
C
C     We pass the sub-array starting at element 2 like this:
      call sub1(X(2), 2)
      write (*,*) 'X=', X
      
      end

C     All subroutine arguments are passed by reference, so we can modify
C     a and send it back to the main program....
      subroutine sub1(a, n)
C      Use the (*) notation to indicate the array length is NOT known
       real a(*)
       integer n
       do 10 i=1,n
        a(i) = a(i) + 1
 10    continue
       return
      end

C     You can pretend that an array is a different size than it really is
      subroutine sub2(a)
C      We tell the subroutine that the array is 2x2..  This technique is
C      used frequently to treat a multidimensional array as a one
C      dimensional one to make algorithms faster and easy to implement.
       real a(2,2)
       do 10 i=1,2
          do 20 j=1,2
             a(i, j) = a(i, j) + (i+j)
 20       continue
 10    continue
       return
      end


