/**
   @file      barrierC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     Demonstrate MPI_Barrier.@EOL
   @Keywords  LAM MPICH OpenMPI MPI
   @Std       C89 MPI1

              This simple little program is closely related to
              the C bindings "hello world" program.  It is intend
              to illustrate the MPI_Barrier function for application
              synchronization.

              Not all platforms allow console I/O on all MPI
              processes.  On such platforms, this program can fail.
*/

#include <stdio.h>              /* I/O lib         ISOC  */
#include <mpi.h>                /* MPI Std         MPI   */

int main(int argc, char *argv[]) {
  int rank, size;

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  printf("I am %d of %d (sleep for %d seconds)\n", rank, size, rank/2+1);

  /* Sleep for a moment to help demo things.. */
  sleep(rank/2+1);

  /* The MPI_Barrier function provides for simple synchronization
     across a communication group.  Every process in the group will
     "stop" on the barrier call until EVERY process gets to the call.
     When they all reach the barrier, all processes start up again. */
  printf("I am %d of %d.  Waiting for barrier...)\n", rank, size);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("I am %d of %d.  Past barrier.  Gonna exit now...)\n", rank, size);

  MPI_Finalize();

  return 0;
} /* end func main */
