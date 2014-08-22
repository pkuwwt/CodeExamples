/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      utime.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to change mtime and atime on a file.@EOL
   @Keywords  timestamp touch file atime mtime
   @Std       C89

   Note that utimes and futimes, both BSDisms, are not portable.  utime is part of POSIX.1 so should be most places.  Note that some
   OSes will allow strange values for the times (negative for example).  Such values will often cause stat to break on other OSes --
   so don't set the time to impossible values.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <utime.h>              /* utime           POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <errno.h>              /* error stf       POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int retV;
  struct utimbuf utimeStruct;

  if(argc != 2) {
    printf("ERROR: A file name is required as the single argument.\n");
    exit(1);
  } /* end if */

  /* Set both to 0 time -- Number of seconds since 0 hours, 0 minutes, 0 seconds, January 1, 1970, Coordinated Universal Time,
     without including leap seconds. */
  utimeStruct.actime  = 0;
  utimeStruct.modtime = 0;

  /* If the last arg is NULL, then both actime and modtime are set to the current time. */
  if(utime(argv[1], &utimeStruct) != 0) { 
    /* -1 is returned upon failure, 0 upon success. Different UNIX variants document different error conditions for this function.
       It is probably best to simply print the perror message.  As with most file system commands, you should retry in case of
       EINTR. */
    printf("ERROR: utime failed with error number: %d\n", errno);
    exit(1);
  } /* end if */
} /* end func main */
