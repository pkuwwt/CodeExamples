/**
   @file      sendRecvC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     Demo MPI functions MPI_Send and MPI_Recv@EOL
   @Keywords  LAM MPI
   @Std       C99 MPI1

              This program demonstrates the following functions:

                    MPI_Send
                    MPI_Recv

              Note that this program makes the same mistake that many
              real world MPI programs do.  It makes the assumption
              that no process will die and that all will end up
              sending back the part of the computation they were
              responsible for. In this program, if that doesn't
              happen, then the rank 0 process will wait forever for a
              communication that may never come.  See sendRecvErrC.c
              for a way out of this situation.

              This code requires that the rank zero process have
              stdout capabilities -- which most MPI implementations
              provide.
*/

#include <stdio.h>              /* I/O lib         ISOC  */
#include <mpi.h>                /* MPI Std         MPI   */

int main(int argc, char *argv[]) {
  int rank, size, n, rslt, i, flags;
  MPI_Status status;
  int needData, msgProbeCount=0;

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  if(rank == 0) {
    printf("Mr. Zero has come on line....\n");
    printf("Mr. Zero waiting for %d slaves to check in.\n", size);

    /* NOTE: If a slave process were to die before sending it's
       results back, this loop would hang forever.  See sendRecvErrC.c
       for an example of how to recover in that situation.*/
    for(i=1;i<size;i++) {
      
      /* Receive a message. */
      rslt = MPI_Recv(&n, 1, MPI_INT, // Location of data, number of elements, type
                      MPI_ANY_SOURCE, // Who to receive data from
                      MPI_ANY_TAG,    // Tags to receive
                      MPI_COMM_WORLD, // Group from which to receive
                      &status);       // Status of call
      printf("Process %d has checked in (%d,%d,%d,%d)\n", 
             n, status.MPI_SOURCE, status.MPI_TAG, status.MPI_ERROR, rslt);
    } /* end for */
  } else {
    /* Sleep a bit to help demonstrate the probe and wait code in the
       rank 0 process. */
    sleep(rank/2+1);

    /* Send a message. MPI_Ssend(...) is the same thing, but it blocks
       until the receiver starts to read data. */
    MPI_Send(&rank, 1, MPI_INT, // The data, number of elements, and type
             0,                 // Who to send data too
             999,               // Tag to use (random number in this case)
             MPI_COMM_WORLD);   // The group we are sending to.
  } /* end if/else */

  MPI_Finalize();
  return 0;
} /* end func main */
