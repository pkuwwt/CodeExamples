/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      randFileC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1990 by Mitch Richling.  All rights reserved.
   @brief     Implementation of the randFileC library@EOL
   @Keywords  random numbers files
   @Std       C89

   See the header file for a general description.
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */

#include <unistd.h>             /* UNIX std stf    POSIX */
#include <fcntl.h>              /* UNIX file ctrl  UNIX  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
int newFileRand(char *fileName) {
  int FD;
  return (FD = open(fileName, O_RDONLY));
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
int delFileRand(int FD) {
  return close(FD);
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
char genFileRandCharS(int FD) {
  char retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
char genFileRandCharSNN(int FD) {
  char retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  if(retValue < 0) retValue = -retValue;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
char genFileRandCharU(int FD) {
  unsigned char retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
int genFileRandIntS(int FD) {
  int retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
int genFileRandIntSNN(int FD) {
  int retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  if(retValue < 0) retValue = -retValue;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
int genFileRandIntU(int FD) {
  unsigned int retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
long genFileRandLongS(int FD) {
  long retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
long genFileRandLongSNN(int FD) {
  long retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  if(retValue < 0) retValue = -retValue;
  return retValue;
}

/* ------------------------------------------------------------------------------------------------------------------------------ */
long genFileRandLongU(int FD) {
  unsigned long retValue;
  if(FD < 0) return 0;
  if(read(FD, &retValue, sizeof(retValue)) < sizeof(retValue)) return 0;
  return retValue;
}
