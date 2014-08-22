CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/types.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates type declarations in Fortran 77.@EOL
C  @Keywords  type declarations FORTRAN f77
C  @Std       F77
C

       program types

C      Non-standard, but common, way to turn off automatic variable types
       IMPLICIT NONE

C      By default vars that start with i, j, k, m, n are ints.
C      Everything else is a real by default.  This can be changed
C      with &&&&&&

C      Various basic data type declarations
       character          c
       logical            l
       real               r
       integer            i
       double precision   d
       complex            cplx
       character * 10     s

C      Some size specific types that are common, but this stuff
C      is not completely standardized.  Characters and logicals
C      can often be one or two bytes as well...
C      real*4     -- Commonly IEEE single (32-bit)
C      real*8     -- Commonly IEEE double (64-bit)
C      real*10    -- Commonly IEEE extended (80-bit) x87 hardware mostly
C      real*16    -- Commonly IEEE quad double (128-bit)
C      integer*1  -- 8-bit signed integer
C      integer*4  -- 32-bit signed integer
C      integer*8  -- 64-bit signed integer
C      complex*8  -- Each part a real*4
C      complex*16 -- Each part a real*8
C      complex*32 -- Each part a real*16

C      Various assignment statements for different types
       c = '1'
       l = (1 .eq. 1)
       r = 1.234
       i = 12345
       d = 1.2345678
       cplx = (1, 2)
       s = '1234567890'

       end
