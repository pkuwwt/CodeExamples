/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      getoptP.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999,2004 by Mitch Richling.  All rights reserved.
   @brief     Demo POSIX getopt() function.@EOL
   @Keywords  posix getopt unix
   @Std       C89 POSIX.2

   How to use the most traditional form of the UNIX getopt() function.  Note the GNU version has significantly more functionality.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */

/**********************************************************************************************************************************/
void prtUseAndExit();

/**********************************************************************************************************************************/
int main(int argc, char **argv) {
  /* Stuff from the getopt library. */
  extern char *optarg;  /* String argument following a regular arg. (-o foo)*/
  extern int   optind;  /* Index of next argument, or of first arg NOT processed. */
  extern int   optopt;  /* Holds error character. */
  extern int   opterr;  /* Set to zero to suppress error messages. */

  /* Our variables */
  int c;
  int curArg;
  int aCmdLineOptFound = 0;
  int bCmdLineOptFound = 0;
  int oCmdLineOptFound = 0;
  char *oCmdLineOptArg = NULL;

  /* Run through the arguments. Note as of IEEE Std 1003.2-1992 (POSIX.2), getopt returns -1 -- was an EOF in the good old days. */
  while ((c = getopt(argc, argv, "habo:")) != EOF) {
    switch (c) {
      case 'a': aCmdLineOptFound++;                          break;
      case 'b': bCmdLineOptFound++;                          break;
      case 'o': oCmdLineOptFound++; oCmdLineOptArg = optarg; break;
      case 'h': prtUseAndExit();                             break;
      case '?': prtUseAndExit();                             break;
      default : prtUseAndExit(); /* Can't happen */          break;
    } /* end swtich */
  } /* end while */

  /* Print out what we found. */
  if(aCmdLineOptFound) { printf("Found(%d): -a\n", aCmdLineOptFound); } /* end if */
  if(bCmdLineOptFound) { printf("Found(%d): -b\n", bCmdLineOptFound); } /* end if */
  if(oCmdLineOptFound) { printf("Found(%d): -o %s\n", oCmdLineOptFound, oCmdLineOptArg); } /* end if */

  /* Go through the rest of the arguments and print them. */
  if(optind < argc) {
    printf("The remaining arguments: \n");
    for (curArg=optind; curArg<argc; curArg++)
      printf("  %s\n", argv[curArg]);
  } /* end if */

  return 0;
} /* end func main */

/**********************************************************************************************************************************/
void prtUseAndExit() {
  fprintf(stderr, "usage: cmd [-h] [-a] [-b] [-o <filename>] files...\n");
  exit(2);
} /* end func prtUseAndExit */
