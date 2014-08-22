!  Author:   Mitch Richling<http://www.mitchr.me/>
!  IP:       Copyright 1999 by Mitch Richling.  All rights reserved.
!  Key word: MPI LAM MPICH
!  Std:      F90 MPI1
!  Notes:    This is a "hello world" program for MPI using the
!            Fortran 90 bindings.  This version is almost identical
!            to the f77 version, except we make use of the header.
!            
!            Not all platforms allow console I/O on all MPI
!            processes, so this program can fail on some platforms.

      program mpihel
      implicit none

      include 'mpif.h'

      integer :: rank, size, mpierr

      call MPI_INIT(mpierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, mpierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, mpierr)

      write (*,*) 'I am ', rank, ' of ', size

      call MPI_FINALIZE(mpierr)      

      end
