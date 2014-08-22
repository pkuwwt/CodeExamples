/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      environ.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     How to use environ@EOL
   @Keywords  environment unix environ
   @Std       ISOC 
   @Tested    
              - Solaris 2.8 
              - MacOS X.2
              - Linux (RH 7.3)
   
   This is an example program intended to illustrate how to use environ.  environ is actually an ISO C construct.  Most UNIX systems
   allow a third parameter to main() that is a pointer to environ; however, the ISO C standard forbids this.  Thus, one really
   should simply use the global variable environ.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
extern char **environ;

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char **cp;
  printf("Environment:\n");
  for(cp=environ; *cp != NULL; cp++)
    printf("  %s\n", *cp);
  printf("\n");
  return 0;
} /* end func main */
