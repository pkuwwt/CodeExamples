/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      readdir.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX directories and directory processing@EOL
   @Keywords  UNIX filename directory UFS
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)
              - Debian 7.4 + gcc (Debian 4.7.2-5) 4.7.2

   This C program is intended to illustrate how one read UNIX directories(filenames in a subdirectory).  This code is quite
   portable.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <dirent.h>             /* UNIX dirs       POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char dirToRead[] = "/";
  DIR *dp;
  struct dirent *dep;
  int errorCount, haveMore;
  const int MAXERRORCOUNT = 10;

  /* First we open the directory.  This function returns a POINTER.  It will return NULL if it fails. */
  dp = opendir(dirToRead);
  if(dp == NULL) {
    printf("ERROR: Could not open directory(%s).\n", dirToRead);
    exit(10);
  } /* end if */

  /* Read the directory entries.  We continue until we have had MAXERRORCOUNT errors or we have no more directory entries(EOD). */
  errorCount = 0;
  haveMore = 1;
  do {
    errno = 0;                  /* You must clear this before calling dep. */
    dep = readdir(dp);
    if(dep == NULL) {           /* We get NULL on error or EOD. */
      if(errno) {               /* If errno is set, it was an error. */
        errorCount++;
        perror(NULL);
      } else {                  /* No errno, but NULL.  That's all.. */
        haveMore = 0;
      } /* end if/else */
    } else {
      printf("File: %s\n", dep->d_name);
    } /* end if/else */
  } while((errorCount < MAXERRORCOUNT) && (haveMore));

  /* We must close the directory just like a file.  Just as in the case of a file close, the close may fail.  Most people don't
     check this for errors. */
  if(closedir(dp) < 0) {
    printf("ERROR: Could not close directory(%s).\n", dirToRead);
    exit(10);
  } /* end if */

  exit(0);
} /* end func main */
