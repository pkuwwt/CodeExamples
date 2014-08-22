/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      uideuid.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     UNIX UID queries@EOL
   @Keywords  UNIX user id uid
   @Std       ISOC POSIX SYSV BSD43
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)
   
   This is an example program intended to illustrate how to query for user ID and effective user ID.
***********************************************************************************************************************************/

#include <unistd.h>             /* UNIX std stf    POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  uid_t theUid, theEuid;

  /* Figure out who the user is running this thing. */
  theUid = getuid();
  theEuid = geteuid();
  printf("uid: %ld  euid: %ld\n", (long)theUid, (long)theEuid);

  return 0;
} /* end func main */
