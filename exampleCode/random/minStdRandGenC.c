/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      minStdRandGenC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997, 2001, 2006, 2014 by Mitch Richling.  All rights reserved.
   @brief     minimal implementation of the minimal standard random number generator@EOL
   @Keywords  none
   @Std       C99

   Generate Random Numbers via the "minimal standard" Linear Congruence Generator (LCG)
   
   This is just a simple LCG with a=16807 & m=2147483647.  The idea of the LCG was first proposed by Lehmer (1949/1951). This choice
   of parameters was first suggested by Lewis, Goodman, & miller (1969).  Later, Park and Miller (1998) provided better grounds for
   it's use and suggested it as a "minimal standard generator".  The name stuck, and they are generally given credit for the choice
   of parameters. Note that different values, probably better ones, have been suggested.  For example, Park & Miller (1988) suggest
   a=48271 or a=69621 because of better spectral test results for randomness -- they have a much tighter hyperplane spacing when
   consecutive points are plotted.  Still, the minimal standard is widely used.
   
   The random numbers are integers.  The period is 2^(31)-1.  The random numbers are uniformly distributed.
   
   The period of this generator is is 2^(31)-1; however, it is best to stick with sequences shorter than 46340.  For more
   information see L'Ecuyer & Hellekalek (1998) and L'Ecuyer, Cordeau, & Simard (2000).
   
   The method of overflow avoidance follows the ideas of L'Ecuyer (1988) and Scharage (1979).  Note: The algorithm presented by
   Carta (1990) is quite bad, and should be avoided.
   
   Learmonth & Lewis (1973) provide extensive tests of this particular generator.
   
   James Gentle's book "Random Number Generation and Monte Carlo Methods", 2nd Ed, 2005, provides a good overview of all the above
   references and solid coverage of this algorithm.  I highly recommend it.
   
   This random number generator is sufficient for many non-critical applications.  For example, while it exhibits the lattice
   structure all LCGs suffer from, it is fine enough to use for Monte Carlo integration of well behaved functions. It can also form
   a fine base for shuffle & shift filters.
   
   History:
     - Aug 1997: Original version
     - Jul 2000: Added doxygen tags to top comment
     - Dec 2001: Added safe period comments
     - Feb 2006: Added comment regarding Gentle's book Changed comments to c99 style
     - Apr 2014  Changed to 132 cols
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {

  long i, randN, q, r;

  long m = 2147483647L;  // 2**(31)-1 for MSLCG
  long a = 16807;        // 7**5 for MSLCG

  q     = m/a; // 127773 for MSLCG
  r     = m%a; //   2836 for MSLCG    
  randN = 1;   // Set the seed (first random number)

  for(i=0; i<10; i++) {
    // Simple (bad)    : randN = (a*randN)%m
    // L'Ecuyer (1988) : k = randN/q; randN = a * (randN-k*q) - r*k;
    // Scharage (1979) : randN = a*(randN%q) - r*(randN/q);
    randN = a*(randN%q) - r*(randN/q);
    if(randN<0)
      randN+=m;
    printf("%12ld\n", randN);
  }

  return 0;
}
