/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      prngEx.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2004 by Mitch Richling.  All rights reserved.
   @brief     Example showing basic prng use@EOL
   @Keywords  prng random number generator example
   @Std       C99

   PRNG is from the people at pLib, and it implements most of the classical random number generators. It is a great package for
   studying older generators as well as for production use in serious simulation work.  I have found the code quality to be quite
   high.  

   It has facilities to seed and reseed all generators.  It also has the ability to skip ahead for some generators.  The interface
   is quite clean in version 3 and beyond.

   You can find it here:

       http://statmath.wu.ac.at/prng/index.html
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <prng.h>

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {

  int i;
  struct prng *g; 
  prng_num randInt; 
  double randDbl, *randDblArr; 
  int numArrNums; 

  /* Create a random number generator
     Typical arguments:
       lcg(2147483647,16807,0,1)  Minimal standard.  Last arg is seed
       eicg(2147483647,111,1,0)
       mt19937(1)                 Arg is seed */
  if((g = prng_new("lcg(2147483647,16807,0,1)")) == NULL) { 
    fprintf(stderr, "ERROR: Call to prng_new failed.\n"); 
    exit (1); 
  } 

  /* Print out some info about the generator */
  printf("Short name: %s\n",prng_short_name(g)); 
  printf("Long name:  %s\n",prng_long_name(g)); 
  if(prng_can_seed(g))
    printf("This generator supports prng_seed()\n");
  else
    printf("This generator doesn't support prng_seed()\n");
  if(prng_can_fast_sub(g))
    printf("This generator supports prng_get_sub_def()\n");
  else
    printf("This generator doesn't support prng_get_sub_def()\n");
  if(prng_can_fast_con(g))
    printf("This generator supports prng_get_con_def()\n");
  else
    printf("This generator doesn't support prng_get_con_def()\n");
  if(prng_is_congruential(g))
    printf("This generator supports prng_get_next_int() & prng_get_modulus()\n");
  else
    printf("This generator doesn't support prng_get_next_int() or prng_get_modulus()\n");

  /* Set the seed if we can */
  if(prng_can_seed(g))
    prng_seed(g, (prng_num)1);

  printf("5 random ints from U[1,%lu) generated via prng_get_next_int:\n", (unsigned long)prng_get_modulus(g));
  if(prng_is_congruential(g)) {
    for(i=0; i<5; i++) {
      randInt = prng_get_next_int(g);
      printf("%3d %lu\n", i, (unsigned long)randInt);
    }
  }

  /* Get a random number in U[0,1) */
  printf("5 random doubles from U[0,1) generated via prng_get_next():\n");
  for(i=0; i<5; i++) {
    randDbl = prng_get_next(g);
    printf("%3d %f\n", i, randDbl);
  }

  /* Get an entire randDblArr (numArrNums long) of random numbers from U[0,1) */
  numArrNums = 5;
  randDblArr = NULL;
  randDblArr = (double *)malloc(sizeof(double)*numArrNums);
  if(randDblArr == NULL) {
    printf("ERROR: Call to malloc() failed.\n");
    exit(3);
  }
  prng_get_array(g, randDblArr, numArrNums);
  if(randDblArr == NULL) {
    printf("ERROR: Call to prng_get_array() failed.\n");
    exit(2);
  }
  printf("%d random doubles from U[0,1) generated via prng_get_array():\n", numArrNums);
  for(i=0; i<numArrNums; i++)
    printf("%3d %f\n", i, randDblArr[i]);
  free(randDblArr);
  
  /* reset the generator */ 
  prng_reset(g);

  /* Clean up resources used by the generator */
  prng_free(g);

  return 0;
} /* end function: main */

// if (prng_can_fast_sub(g)) 
//   puts(prng_get_sub_def(g,20,0)); /* Get sub-sequence definition */ 
//
// if (prng_can_fast_con(g)) 
//   puts(prng_get_con_def(g,20,1)); /* Get block definition 



