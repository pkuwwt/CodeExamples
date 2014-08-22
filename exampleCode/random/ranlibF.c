/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      ranlibF.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     C wrapper implementation for ranlib@EOL
   @Keywords  ranlib Fortran c wrapper
   @Std       C89
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include "ranlibF.h"

/* ------------------------------------------------------------------------------------------------------------------------------ */
/* Prototypes for functions in Fortran library. */

void        getcgn_(for_integer *gennum);
void        setcgn_(for_integer *gennum);
void        setall_(for_integer *seed1, for_integer  *seed2);
void        setsd_(for_integer  *seed1, for_integer  *seed2);
void        getsd_(for_integer *seed1, for_integer *seed2);
for_integer ignlgi_();
for_real    ranf_();
for_real    gennor_(for_real *mean, for_real *sd);

/* ------------------------------------------------------------------------------------------------------------------------------ */
/* Implementation of C wrappers. */

void getcgn_F(for_integer *gennum) {
  getcgn_(gennum);
}

void setcgn_F(for_integer gennum) {
  setcgn_(&gennum);
}

void setall_F(for_integer seed1, for_integer  seed2) {
  setall_(&seed1, &seed2);
}

void setsd_F(for_integer  seed1, for_integer  seed2) {
  setsd_(&seed1, &seed2);
}

void getsd_F(for_integer *seed1, for_integer *seed2) {
  getsd_(seed1, seed2);
}

for_integer ignlgi_F() {
  return ignlgi_();
}

for_real ranf_F() {
  return ranf_();
}

for_real gennor_F(for_real mean, for_real sd) {
  return gennor_(&mean, &sd);
}
