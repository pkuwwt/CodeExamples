/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      ranlibExC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     Example of RANLIB@EOL
   @Keywords  example of ranlib c
   @Std       C89

   Generally speaking, I suggest wrapping the Fortran version of RANLIB in nice C wrappers over making use of the C version of the
   library -- the translation is adequate; however, not expertly done.  Some of the interfaces are awkward. Still, for simple needs,
   the C version is, as I said before, adequate.  Illustrated below are the most commonly used functions.  See the Fortran example
   for detailed notes regarding function use.
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <ranlib.h>

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {

  int    j;
  long   seed1, seed2, ranInt;
  double ranReal;

  /* Initialize all generators */
  setall(11, 9);

  /* Set the seed for the current generator*/
  setsd(11, 9);

  /* Query to see what the seed is for current generator */
  getsd(&seed1, &seed2);
  printf("Seed after SETSD: %ld %ld\n", seed1, seed2);

  printf("Five random integers in U[1, 2147483562] from the current generator:\n");
  for(j=1; j<6; j++) {
    ranInt = ignlgi();
    printf("%3d %20ld\n", j, ranInt);
  }

  printf("Five random reals in U(0,1) from the current generator:\n");
  for(j=1; j<6; j++) {
    ranReal = ranf();
    printf("%3d %20.10f\n", j, ranReal);
  }

  printf("Five random reals in N(1,0.0001) from the current generator:\n");
  for(j=1; j<6; j++) {
    ranReal = gennor(1.0, 0.0001);
    printf("%3d %20.10f\n", j, ranReal);
  }

  return 0;
}
