/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      gidegid.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     UNIX gid query functions@EOL
   @Keywords  UNIX group id
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)              

   This is an example program intended to illustrate how to query for user ID and effective user ID.
***********************************************************************************************************************************/

#include <unistd.h>             /* UNIX std stf    POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  gid_t theGid, theEgid;

  /* Figure out who the user is running this thing. */
  theGid = getgid();
  theEgid = getegid();
  printf("gid: %ld  egid: %ld\n", (long)theGid, (long)theEgid);

  return 0;
} /* end func main */
