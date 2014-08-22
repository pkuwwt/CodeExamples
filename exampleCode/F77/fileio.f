CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  @file      /home/richmit/world/my_prog/learn/F77/fileio.f
C  @Author    Mitch Richling<http://www.mitchr.me>
C  @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
C  @brief     Illustrates simple file I/O in Fortran 77.@EOL
C  @Keywords  file i/o fortran f77
C  @Std       F77
C

        program fileio

       integer i

C      The first * in 'write (*,*)' or 'read (*,*)' is
C      a 'unit' number (like a file descriptor in UNIX lingo)


C      You get to pick the unit number of the file that is
C      opened (one of FORTAN's oddities).  Note that old
C      programmers use 5 for input and 6 for output -- Fortran
C      IV requires 5/6 (not *'s).  Several tags exist for the open
C      call:
C         UNIT=u       -- unit number
C         FILE=name    -- 'INPUT'   - STDIN
C                         'OUTPUT'  - STDOUT
C                                   - A file name
C         STATUS=stat  -- 'OLD'     - The file must already exist
C                         'NEW'     - File must not exist already
C                         'SCRATCH' - Nuke it at program end
C                         'UNKNOWN' - Platform dependent
C         ACCESS=acc   -- 'SEQUENTIAL - 
C                         'DIRECT'     
C         FORM=fm      -- 'FORMATTED'   The Default for 'SEQUENTIAL'
C                         'UNFORMATTED' The Default for 'DIRECT'
C         RECL=rl      -- positive int representing record length
C         BLANK=blnk   -- 'NULL' - blanks in numeric fields ignored (default)
C                         'ZERO' - blanks are treated as zeros.
C         ERR=s
C         IOSTAT=ios
       open(UNIT=2, FILE='outf')

C      I always rewind files before i do anything with them.
C      The 'UNIT=' part is optional
       rewind(UNIT=2)

C      Use the unit number as the first *-thingy.
       do 100 i=1,10
          write (2,*) i
 100   continue

C      Close the file with CLOSE. The STATUS can be one of
C        UNIT=u      -- File unit.  The 'UNIT=' part can be omitted
C        STATUS=stat -- 'DELETE' - Delete the file upon program end
C                       'KEEP'   - Keep the file after program end (default)
C        IOSTST=ios  
C        ERR=s
       CLOSE(UNIT=2, STATUS='KEEP')

C      We can read in the file in a similar way.  Note that we
C      use the END=250 construct to jump to line 250 when we
C      hit EOF -- otherwise we would STOP.
       open(UNIT=2, FILE='outf')
 200   read (2,*,END=250) i
       write (*,*) 'Got a value=', i
       goto 200
 250   continue
       write (*,*) 'Done..'

       end
