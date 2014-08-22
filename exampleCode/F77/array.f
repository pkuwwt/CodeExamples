CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/array.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @Keywords  FORTRAN f77 array
C  @brief     Illustrates some array concepts in FORTRAN 77.@EOL
C            
        program array

C      Note, you can use a declaration and a dimension statement
C      or do it all in a declaration.
C        real X
C        dimension X(10)
C      is the same as
C        real X(10)

C      One must allocate an array.  The dimension statement
C      MUST appear before any executable code statements.
C      Most people don't use dimension, they just declare
       integer iarray(3)

C      Here we use a FORTRAN 77 feature and specify a zero-indexed
C      array, and an array indexed from -3 to 4
       real ziarray(0:4)
       integer niarray(-3:4)

C      Many dimension arrays are possible.  The two following are 2D
       real twoarray(5,3), twoarray2(2:3, 4)

C      An expression may also be the used for one of the endpoints or
C      the size
       integer muarray(3*4)
       real zimuarray(0:3*4)

C      You can print an entire array like this:
       write (*,*) 'The array', iarray

C      You can read an entire array like this:
       write (*,*) 'Enter three values:'
       read *, iarray

C      You access a single element of an array like this:
       write (*,*) 'Element 3 of iarray is:', iarray(3)

       end
