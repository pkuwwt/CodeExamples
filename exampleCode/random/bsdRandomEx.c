/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      bsdRandomEx.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2004 by Mitch Richling.  All rights reserved.
   @brief     Example for the BSD random/srandom functions@EOL
   @Keywords  random bsd 
   @Std       C89

   In BSD 4.2 the random() and srandom() functions appeared as a better replacement for the ISO C rand()/srand() functions and the
   family of rand48() functions.  random() has a period of roughly 16*((2**31)-1), and returns long integers roughly distributed as
   U[0, (2**31)-1].  In addition to replacing rand() and srand(), the BSD developers also provided an interface to the random device
   via srandomdev() and a more sophisticated state management system via setstate() and initstate().  The last two functions are not
   demonstrated here.  Note that random() produces better random numbers than rand(), but one should still use more secure sources
   for cryptographic needs and better studied sources for serious simulation.
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]);

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {

  int  i;
  long aRand;

  /* Set the seed for the random number */
  srandom(1234);

  printf("Five random long integers in U[0, (2**31)-1]\n");
  for(i=0; i<5; i++) {
    aRand = random();
    printf("%3d %15ld\n", i, aRand);
  }

  /* Use the random number device to seed the generator.  Note that
     this can lead to states no reproducible via srandom() */
  srandomdev();

  printf("Five random long integers in U[0, (2**31)-1]\n");
  for(i=0; i<5; i++) {
    aRand = random();
    printf("%3d %15ld\n", i, aRand);
  }

  return 0;
}
