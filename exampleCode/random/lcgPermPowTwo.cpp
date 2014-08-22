// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
   @file      lcgPermPowTwo.cc
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
   @brief     Special LCG to permute objects@EOL
   @Keywords  LCG string permute
   @Std       C++

   The choice of a=5 & c=9 in a linear congruence random number generator will be of full modulus when m=2^n for some n, and will
   have no fixed points when the initial seed is set to 0.  A fixed point is when the k'th random number in the sequence is equal to
   k.
   
   Thus we have a choice of parameters that will permute around an object, like a sting, with length that is a power of two. This
   has been tested with m = 4, 8, 16, 32, 64, ..., 2^20.
   
   We demonstrate this property with a 128 byte string.  The string in question is a handy quote by David Hilbert that just happens
   to be precisely 128 bytes long.  We continue to permute the string until it returns to it's original form.  We only display the
   first and last four strings in the sequence.
              
*/

//----------------------------------------------------------------------------------------------------------------------------------
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

//----------------------------------------------------------------------------------------------------------------------------------
int main(int argc, char *argv[]) {
  char someText[129] = "If I were to awaken after having slept for a "
                       "thousand years, my first question would be: "
                       "Has the Riemann hypothesis been proven?";
  char cTmp;

  int a = 5;
  int c = 9;
  int m = 128;

  for(int j=1; j<4097; j++) {
    /* Print first and last four strings */
    if( (j<5) || (j>4092) )
      printf("%5d: %s\n", j, someText);
    /* Permute the string */
    int r = 0;
    for(int i=0; i<m; i++) {
      /* Compute our "random" number */
      r = (a * r + c) % m;
      /* Print out any fixed points we find */
      if(i == r)
        printf("FIXED POINT: %3d %3d\n", i, r);
      /* Swap elements */
      cTmp = someText[i];
      someText[i] = someText[r];
      someText[r] = cTmp;
    }
  }

  return 0;
}
