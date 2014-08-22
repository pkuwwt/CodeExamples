/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      solaris_ps.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     How to access process info in Solairs (like /bin/ps)@EOL
   @Keywords  UNIX Solaris proc procfs ps psinfo
   @Std       ISOC Solaris
   @Tested    
              - Solaris 2.7
              - Solaris 2.8
              - Solaris 2.9
              - BROKEN: Linux   -- incompatable /proc
              - BROKEN: MacOS X -- no /proc
              - BROKEN: Solaris 2.5.x

   This C program is intended to illustrate how one can query process information available through the procfs file-system available
   in Solaris.
   
   /proc contains directories with numeric names corresponding to the PIDs of processes currently active on the system.  Each
   directory contains several files that have various bits of information about each process.  In older versions of Solaris, pre
   2.6, the content of /proc was simply regular files providing the same information through the use of ioctl() calls.  This program
   only works with newer versions of Solaris that use directories.  In particular, this program accesses the psinfo file contained
   in each directory.  The UID running this program must have access to the directories(root for example).  The exit method is not
   very clean and the read call is also not very clean...
***********************************************************************************************************************************/

#include <dirent.h>             /* UNIX dirs       POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <fcntl.h>              /* UNIX file ctrl  UNIX  */
#include <procfs.h>             /* Solaris proc    SUN   */
#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int fd;
  char dat[512];
  struct psinfo *thePsInfo;
  char dirToRead[] = "/proc";
  DIR *dp;
  struct dirent *dep;
  int errorCount, haveMore;
  char fileToOpen[1024];

  dp = opendir(dirToRead);
  if(dp == NULL) {
    printf("ERROR: Could not open directory(%s).\n", dirToRead);
    exit(10);
  } /* end if */

  errorCount = 0;
  haveMore = 1;
  do {
    errno = 0;
    dep = readdir(dp);
    if(dep == NULL) {
      if(errno) {
        errorCount++;
        perror(NULL);
      } else
        haveMore = 0;
    } else {
      sprintf(fileToOpen, "/proc/%s/psinfo", dep->d_name);

      fd = open(fileToOpen, O_RDONLY);
      if(fd >= 0) {
        read(fd, dat, 416);
        thePsInfo = (struct psinfo *)dat;
        printf("PID: %-10s  CMD: %s \n", dep->d_name, thePsInfo->pr_fname);
        close(fd);
      } /* end if */
    } /* end if/else */
  } while((errorCount < 10000) && (haveMore));

  if(haveMore) {
    printf("Exit due to repeated errors.\n");
  } /* end if */

  if(closedir(dp) < 0) {
    printf("ERROR: Could not close directory(%s).\n", dirToRead);
    exit(10);
  } /* end if */

  exit(0);
} /* end func main */
