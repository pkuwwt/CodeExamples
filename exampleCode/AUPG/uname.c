/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      uname.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     How to use uname@EOL
   @Keywords  unix uname
   @Std       ISOC 
   @Tested    
              - MacOS X.4
              - Debian 7.4 + gcc (Debian 4.7.2-5) 4.7.2
   
   This is an example program intended to illustrate how to use the POSIX function uname.
***********************************************************************************************************************************/

#include <sys/utsname.h>        /* uname           POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct utsname utsnameStructVar;

  if(uname(&utsnameStructVar) == -1) {
    printf("ERROR(%d): uname failed.\n", (int)errno);
  } /* end if */

  /* Note that the nodename and sysname may NOT be enough to identify the host on a network -- i.e. they may not even be related to
     the hostname, but it generally is. */
  printf("sysname(-s):  %s\n", utsnameStructVar.sysname);
  printf("nodename(-n): %s\n", utsnameStructVar.nodename);
  printf("release(-r):  %s\n", utsnameStructVar.release);
  printf("version(-v):  %s\n", utsnameStructVar.version);
  printf("machin(-m):  %s\n", utsnameStructVar.machine);

  return 0;
} /* end func main */
