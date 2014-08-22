CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      ifth.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     if-then-like constructs in FORTRAN 77.@EOL
C  @Keywords  if then else endif FORTRAN f77
C  @Std       F77
C

        program ifth

C       The if/then
        if ( 1 .le. 3) then
          write (*,*) '1 .le. 3'
        endif

C       The if/then/else
        if ( 1 .eq. 3) then
          write (*,*) '1 .eq. 3'
        else
          write (*,*) 'NOT: 1 .eq. 3'
        endif

C       The if/then-if/else
        if ( 1 .eq. 3) then
          write (*,*) '1 .eq. 3'
        else if ( 1 .le. 3) then
          write (*,*) '1 .le. 3'
        else 
          write (*,*) 'NOT: (1 .eq. 3) .and. (1 .le. 3)'
        endif

C       The LOGICAL if statement from FORTRAN IV
        if (2 .le. 3) write (*,*) '2 .le. 3'

C      Boolean binary operators
C        .eq.   -- equals
C        .ne.   -- NOT equals
C        .gt.   -- greater than
C        .lt.   -- less than
C        .ge.   -- greater than or equal
C        .le.   -- less than or equal
C      Logical Operations
C        .and.  -- and
C        .or.   -- or
C        .not.  -- Logical negation of following expression
C        .eqv.  -- LOGICAL equivalence
C        .neqv. -- LOGICALLY NOT equivalent

       end
