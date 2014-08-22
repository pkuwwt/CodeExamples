/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      ranlibExFC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     Example of RANLIB@EOL
   @Keywords  example of ranlib c
   @Std       C89

   In ranlibExC.c I suggest wrapping the Fortran version of RANLIB in nice C wrappers over making use of the C version of the
   library.  This can have several advantages:

      - A more comfortable API - more C-like
      - No possibility of different results between the C and Fortran
      - Very similar API for both C and Fortran

   The ranlibExC.c program was different from the Fortran library example (ranlibExF.f90) because the C bindings are different from
   the Fortran bindings.  This set of C wrappers illustrates one way to produce C bindings that are very similar to the Fortran API
   -- allowing easy translation between the two languages.  This C program is a precise duplication of the Fortran version, so for
   more information, see the comments in the Fortran version of the program.
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include "ranlibF.h"

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {

  for_integer  j, genNum, seed1, seed2;
  for_integer  ranInt1, ranInt2, ranInt3;
  for_real     ranReal;

  setall_F(11, 9);

  setcgn_F(2);

  getcgn_F(&genNum);
  printf("Using generator number %11d\n", (int)genNum);

  getsd_F(&seed1, &seed2);
  printf("Seed after SETALL: %11d %11d\n", (int)seed1, (int)seed2);

  setsd_F(11, 9);

  getsd_F(&seed1, &seed2);
  printf("Seed after SETSD: %11d %11d\n",(int)seed1, (int)seed2);

  printf("Five random integers in U[1, 2147483562] from first three generators:\n");
  for(j=0;j<5;j++) {
    setcgn_F(1);
    ranInt1 = ignlgi_F();
    setcgn_F(2);
    ranInt2 = ignlgi_F();
    setcgn_F(3);
    ranInt3 = ignlgi_F();
    printf("%11d %11d %11d %11d\n", j, ranInt1, ranInt2, ranInt3);
  }

  setcgn_F(1);
  printf("Five random reals in U(0,1) from generator 1:\n");
  for(j=0;j<5;j++) {
    ranReal = ranf_F();
    printf("%11d %10.7f\n", j, ranReal);
  }

  setcgn_F(1);
  printf("Five random reals in N(1,0.0001) from generator 1:\n");
  for(j=0;j<5;j++) {
    ranReal = gennor_F(1.0, 0.0001);
    printf("%11d %10.7f\n", j, ranReal);
  }

  return 0;
}

