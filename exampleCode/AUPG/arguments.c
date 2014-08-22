/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      arguments.c
   @author    Mitch Richling <http://www.mitchr.me>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     How to access command line arguments from C@EOL
   @Keywords  UNIX ANSI/ISO C command line arguments
   @Std       ISOC
   @Tested
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to access the command line arguments given to a program.

   This program is actually ANSI/ISO C, and thus the functionality demonstrated is typical of all UNIX versions and several non-UNIX
   operating systems including old MacOS, MSDOS, and MS Windows.

   As a side note: In old versions of C, a third argument to main() was used to pass environment variables.  This is not allowed in
   ISO C.  The functions getenv() and putenv() provide access to the environment in ISO C.  An external variable called environ also
   provides access to the environment.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int i;

  /* argc is always >= 1.  The name of the program is generally argument number 0 */
  printf("Program has %d argument%s (argc==%d).\n", argc-1, (argc != 2 ? "s" : ""), argc);

  for(i = 1; i < argc; i++) {
    printf("Argument #%02d: %s\n", i, argv[i]);
  } /* end for i */

  return 0;
} /* end func main */
