/**
   @file      initStuffC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     Demonstrate@EOL
   @Keywords  LAM MPICH MPI
   @Std       C89 MPI1

              Illustrates a few details missing from the "hello world"
              program including error detection and MPI program abort.
              The following functions illustrated:

                   MPI_Errhandler_set
                   MPI_Abort
                   MPI_Get_processor_name

              Not all platforms allow console I/O on all MPI
              processes, so this program can fail on some platforms.
*/

#include <stdio.h>              /* I/O lib         ISOC  */
#include <mpi.h>                /* MPI Std         MPI   */

int main(int argc, char *argv[]) {
  int rank, size, prNameLen, retV;
  char prName[MPI_MAX_PROCESSOR_NAME];
  char errStr[MPI_MAX_ERROR_STRING];
  int  errLen;

  MPI_Init(&argc, &argv);

  /* The startup error handler is MPI_ERRORS_ARE_FATAL.  This will
     cause all errors to abort the MPI application (all tasks are
     exited).  To get return codes, use MPI_ERRORS_RETURN. */
  MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  printf("INFO(%d,%d): I am alive\n", rank, size);

  if((retV=MPI_Get_processor_name(prName, &prNameLen)) != MPI_SUCCESS) {
    if(retV == MPI_ERR_UNKNOWN) { 
      printf("ERROR(%d,%d): Unknown error getting processor name.\n", rank, size);
    } else {
      MPI_Error_string(retV, errStr, &errLen);
      printf("ERROR(%d,%d): Getting processor name: (%d,%s,%d)\n", rank, size, retV, errStr, errLen);
    } /* end if/else */
    MPI_Abort(MPI_COMM_WORLD, retV);
  } /* end if */

  printf("INFO(%d,%d): This processor is named: %s\n", rank, size, prName);

  MPI_Finalize();

  return 0;
} /* end func main */
