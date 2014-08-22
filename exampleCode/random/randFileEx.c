/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      randFileEx.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1990 by Mitch Richling.  All rights reserved.
   @brief     Demonstrate how to use a random number file@EOL
   @Keywords  random number file
   @Std       C89

   This program shows how to use my randFile library to obtain a random sequence of chars, ints, or longs from a random bit sequence
   file.
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include "randFileC.h"

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {
  int ranGen;
  int i;

  if(argc < 2) {
    printf("ERROR: Must provide a file name as the first argument\n");
    exit(1);
  }

  /* Initialize random number generator. */
  ranGen = newFileRand(argv[1]);

  /* Three sizes of random number: char, int, and long.  
     Three types for each size: S=signed, SNN=signed non-negative, U=unsigned
     Function names constructed like so: genFileRand<size><types>() */
  printf("%3s %7s %7s %7s %12s %12s %12s %22s %22s %22s\n", "idx",
         "charS", "charSNN", "charU", "intS", "intSNN", "intU", "longS", "longSNN", "longU");
  for(i=0; i<25; i++) {
    printf("%3d %7ld %7ld %7ld %12ld %12ld %12ld %22ld %22ld %22lu\n", i, 
           (long)genFileRandCharS(ranGen), (long)genFileRandCharSNN(ranGen), (long)genFileRandCharS(ranGen),
           (long)genFileRandIntS(ranGen),  (long)genFileRandIntSNN(ranGen),  (long)genFileRandIntS(ranGen),
           (long)genFileRandLongS(ranGen), (long)genFileRandLongSNN(ranGen), (long)genFileRandLongS(ranGen));
  }

  /* Free up resources used by random number generator */
  delFileRand(ranGen);

  return 0;
}
