/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      randFileC.h
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1990 by Mitch Richling.  All rights reserved.
   @brief     Include file for the randFileC library@EOL
   @Keywords  random numbers files include
   @Std       C89

   Files full of random bit streams are a common way to preserve random number sequences, obtain cryptographic sequences, or store
   high quality sequences from physical generators.  Once one has a file in hand, one needs to turn it into a stream of random
   numbers usable for simulation.

   This library solves that problem by providing an interface to open multiple random number files and then return the contents of
   the files as a sequence of various integer types.

   Currently no support is provided for real types.  This may be implemented in a future version -- I have a good idea of how to do
   this very nicely for IEEE floating point systems.
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */

int newFileRand(char *fileName);
int delFileRand(int FD);

char genFileRandCharS(int FD);
char genFileRandCharSNN(int FD);
char genFileRandCharU(int FD);

int genFileRandIntS(int FD);
int genFileRandIntSNN(int FD);
int genFileRandIntU(int FD);

long genFileRandLongS(int FD);
long genFileRandLongSNN(int FD);
long genFileRandLongU(int FD);
