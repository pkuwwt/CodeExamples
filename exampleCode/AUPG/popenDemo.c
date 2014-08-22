/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      popenDemo.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     An example of popen@EOL
   @Keywords  UNIX shell system popen pipe
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how to make simple use of the popen(2) system call.  The popen call returns a pointer to
   a FILE, and is thus at home in the ANSI/ISO C stream I/O library.  Being part of the ANSI/ISO C I/O library, it suffers from the
   same limitations.  Further, popen is not POSIX.1 because it requires the shell.  This program takes a single command line
   argument that is executed.  The standard out of the command line is captured, and printed.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  FILE *cmdOutput;
  int ch;

  /* Make sure we have precisely 2 arguments. */
  if(argc != 2) {
    fprintf(stderr, "ERROR: Incorrect enough arguments.\n");
    fprintf(stderr, "Usage: %s <command_line>\n", argv[0]);
    exit(1);
  } /* end if */

  /* Call popen, and save the ANSI/ISO C FILE pointer. */
  cmdOutput = popen(argv[1], "r");
  if(cmdOutput == NULL) {
    perror("ERROR: popen");
    exit(1);
  } /* end if */

  /* Read data from the file pointer until we have no more data.  This is a silly way to read the data, but it works. */
  while((ch = fgetc(cmdOutput)) != EOF) {
    printf("%c", (char)ch);
  } /* end while */

  /* One must use pclose(2) on a popened file pointer. */
  pclose(cmdOutput);

  return 0;
} /* end func main */
