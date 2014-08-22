/**
   @file      helloC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     First MPI program@EOL
   @Keywords  LAM MPICH MPI
   @Std       C89  MPI1

              This is a "hello world" program for MPI using the
              C bindings.  Functions illustrated:

                   MPI_Init
                   MPI_Comm_rank
                   MPI_Comm_size
                   MPI_Finalize

              Not all platforms allow console I/O on all MPI
              processes, so this program can fail on some platforms.
*/

#include <stdio.h>              /* I/O lib         ISOC  */
#include <mpi.h>                /* MPI Std         MPI   */

int main(int argc, char *argv[]) {
  int rank, size;

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  printf("I am %d of %d\n", rank, size);

  MPI_Finalize();

  return 0;
} /* end func main */
