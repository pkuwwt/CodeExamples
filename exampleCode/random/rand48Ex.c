/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      rand48Ex.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Demo program for rand48.@EOL
   @Keywords  iso c random rand srand rand48
   @Std       C89

   Many UNIX systems have a 48-bit random number generator originally made available as part of SysV.  The generator is not
   appropriate for serious simulation work; however, it is generally better than the ISO C rand() function.  Most modern systems
   implementing this function also have random() available -- which is usually a better alternative.
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {

  int    i;
  long   aRandL;
  double aRandD;
    
  /* Set the seed for the random number generator */
  srand48(1234L);

  printf("Five random longs in U[0, 2**31-1]\n");
  for(i=0; i<5; i++) {
    aRandL = lrand48();
    printf("%3d %15ld\n", i, aRandL);
  }

  printf("Five random longs in U[-2**31, 2**31-1]\n");
  for(i=0; i<5; i++) {
    aRandL = mrand48();
    printf("%3d %15ld\n", i, aRandL);
  }

  printf("Five random doubles in U[0.0, 1.0)\n");
  for(i=0; i<5; i++) {
    aRandD = drand48();
    printf("%3d %15f\n", i, aRandD);
  }

  return 0;
}
