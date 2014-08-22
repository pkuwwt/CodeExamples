/**
   @file      reduceC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     Demo the MPI functions bcast and reduce.@EOL
   @Keywords  LAM MIPCH MPI
   @Std       C99 MPI1

              sum(i=2,n,sum(j=0,i,j))

              This program computes the silly nested sum above in
              order to demonstrate the following functions:

                    MPI_Bcast
                    MPI_Reduce
                    MPI_Gather

              This code assumes that the environment provides
              stdin/stdout for the rank 0 process. While most MPI
              implementations do provide this functionality, if yours
              doesn't then this code will fail.
*/
 
#include <stdio.h>              /* I/O lib         ISOC  */
#include <mpi.h>                /* MPI Std         MPI   */

int main(int argc, char *argv[]) {
  int rank, size, n, i, j;
  double locSum, sum;
  double allLocSums[1024];  // We assume not more than 1024 processes...

  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  /*printf("I am %d of %d\n", rank, size);*/

  while(1) {
    if(rank == 0) {
      /* Newline is required for OpenMPI */
      printf("Enter an integer less than %d (0 to quit): \n", size);
      scanf("%d", &n);
      if(n>=size) {
        printf("Integer too big. Using n=%d instead!\n", size);
        n = size;
      } /* end if */
    } /* end if */

    /* Bcast is a strange function in that it broadcasts data and
       receives it!  Arg #4 determines if a process is sending or
       receiving. */
    MPI_Bcast(&n, 1, MPI_INT,   // ptr to data, num elements, type
              0,                // process with master copy
              MPI_COMM_WORLD);  // group to send data too
    if(n==0)
      break;
    else {
      /* Task of rank will compute: sum(j=0, rank, j/rank) (if rank<=n) */
      locSum=0;
      if(rank <= n) {
        for(j=0;j<=rank;j++) {
          locSum += j;
        } /* end for */
      } /* end if */

      /* Just for fun (and to demonstrate gather), we collect the
         partial sums computed by the various slaves.  One might be
         tempted to use gather and sum up what is returned; however,
         this is very bad practice (a rather common bad practice
         unfortunately).  One should use reduce, as demonstrated
         later, for this application.  The reason is that a high
         performance MPI implementations will be able to use tree
         algorithms combined with knowledge of the topological
         considerations of the network to gain higher performance.
         Gather takes the locSum variables from every process and
         places them in the allLocSums array, in the process specified
         arg#7.*/
      MPI_Gather(&locSum,    1, MPI_DOUBLE,  // Var to gather, length, type
                 allLocSums, 1, MPI_DOUBLE,  // Array to gather into, length, type
                 0,                          // Task to gather into
                 MPI_COMM_WORLD);            // Group of processes from which to gather
      if(rank==0)
        for(j=0;j<size;j++) 
          printf("Process %d computed %f\n", j, allLocSums[j]);


      /* This function illustrates some of the special features only
         found in scientific message passing libs -- you won't find
         this stuff in the sockets API.  MPI_Reduce takes the locSum
         variables from every process except the one specified by arg
         #6 and places the sum in the process specified by arg#6. */
      MPI_Reduce(&locSum,             // Data to sum
                 &sum, 1, MPI_DOUBLE, // Place to put sum, number of data points, type
                 MPI_SUM,             // Operation (sum in this case)
                 0,                   // Process to get the sum (&sum)
                 MPI_COMM_WORLD);     // The group of processes from which to sum
      if(rank==0)
        printf("The sum is: %f\n", sum);
    } /* end if/else */
  } /* end while */

  MPI_Finalize();
  return 0;
} /* end func main */
