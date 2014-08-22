C  Author:   Mitch Richling<http://www.mitchr.me/>
C  IP:       Copyright 1999 by Mitch Richling.  All rights reserved.
C  Key word: MPI LAM MPICH
C  Std:      F77 MPI1
C  Notes:    This is a "hello world" program for MPI using the
C            FORTRAN bindings.
C            
C            Not all platforms allow console I/O on all MPI
C            processes, so this program can fail on some platforms.

      program mpihel
C     Not std F77, but part of MIL STD so most compilers have this
      IMPLICIT NONE

C     Not std F77, but most compilers let us do it anyhow...
      include 'mpif.h'

      integer rank, size, mpierr

      call MPI_INIT(mpierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, mpierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, mpierr)

      write (*,*) 'I am ', rank, ' of ', size

      call MPI_FINALIZE(mpierr)      

      end
