/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      ranlibF.h
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     include file for C wrappers of ranlib@EOL
   @Keywords  ranlib Fortran c wrapper include
   @Std       C89
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
/* A couple of typedefs for Fortran compatibility */
typedef float for_real;
typedef int for_integer;

/* ------------------------------------------------------------------------------------------------------------------------------ */
/* The C wrapper prototypes */
void        getcgn_F(for_integer *gennum);
void        setcgn_F(for_integer gennum);
void        setall_F(for_integer seed1, for_integer  seed2);
void        setsd_F(for_integer  seed1, for_integer  seed2);
void        getsd_F(for_integer *seed1, for_integer *seed2);
for_integer ignlgi_F();
for_real    ranf_F();
for_real    gennor_F(for_real, for_real);

