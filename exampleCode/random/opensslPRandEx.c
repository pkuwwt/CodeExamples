/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/* ****************************************************************************************************************************** */
/**
   @file      opensslPRandEx.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     OpenSSL random number generator example.@EOL
   @Keywords  openssl random number generator example
   @Std       C89

   OpenSSL is a cryptographic toolkit containing many useful things including a excellent, cryptographic random number generator and
   a reasonably good pseudo-random number generator.

   The two functions at the center of random number generation are RAND_pseudo_bytes and RAND_bytes.  They both generate multiple
   bytes of random information in one call.  RAND_bytes & RAND_pseudo_bytes are essentially interchangeable if all that is needed is
   a non-secure random number.

   RAND_pseudo_bytes() has been in OpenSSL since 0.9.5.  RAND_bytes() has always been in OpenSSL, and got a return value in v0.9.5
              
*/

/* ------------------------------------------------------------------------------------------------------------------------------ */
#include <openssl/rand.h>
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <stdio.h>              /* I/O lib         ISOC  */

/* ------------------------------------------------------------------------------------------------------------------------------ */
int main(int argc, char *argv[]) {

  int i, randInt;
  int randInts[10];

 /* One way to get random integers -- full range */
 if( !(RAND_pseudo_bytes((unsigned char *)randInts, sizeof(randInts)))) {
   printf("ERROR: call to RAND_pseudo_bytes() failed\n");
   exit(1);
 }

 /* Print out our random integers.  Note we abs() them to fold into non-negative integers.  One might also wish to exclude 0 from
    the stream for obvious reasons */
 for(i=1; i<10; i++) {
   randInt = abs(randInts[i]);
   printf("Random Integer: %d\n", randInt);
 }

 return 0;
}
