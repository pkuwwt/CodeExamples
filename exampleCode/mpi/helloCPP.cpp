/**
   @file      helloCPP.cc
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     First MPI program@EOL
   @Keywords  LAM MPI
   @Std       C++ MPI1

              This is a "hello world" program for MPI using the
              C++ bindings.  Note that not all platforms allow
              console I/O on all MPI processes, so this program
              can fail on some platforms.
*/

#include <iostream> /* Need for cout/endl                       */
#include <mpi.h>    /* Standard MPI header for the C++ bindings */

int main(int argc, char *argv[]) {
  int rank, size;

  MPI::Init(argc, argv);

  rank = MPI::COMM_WORLD.Get_rank();
  size = MPI::COMM_WORLD.Get_size();
  std::cout << "Hello world! I am " << rank << " of " << size << std::endl;

  MPI::Finalize();

  return 0;
} /* end func main */
