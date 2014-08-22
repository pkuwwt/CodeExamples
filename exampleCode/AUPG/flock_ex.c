/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      flock_ex.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to use flock()
   @Keywords  UNIX file lock flock
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - MacOS X.3

   This C program is intended to illustrate how to use flock().  flock() provides an interface for advisory locking that is less
   complex to use than other locking systems.  flock is widely available on across different UNIX systems.  Test this program by
   running two copies at the same time, one started slightly after the first one.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <sys/file.h>           /* Locks           UNIX  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int FD;
  char buf[] = "hello\n";
  int i;

  if((FD = open("foo", O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU|S_IRWXG|S_IRWXO)) < 0) {
    perror("ERROR: File open");
    exit(10);
  } /* end if */

/*
  LOCK_SH shared lock 
  LOCK_EX exclusive lock 
  LOCK_NB don't block when locking 
  LOCK_UN unlock 
*/

  printf("Attempting to get lock.\n");
  /* NOTE: Use LOCK_SH for shared lock.  The LOCK_NB part keeps the call from blocking, and gives us the chance to make intelligent
     decisions in the face of a previously locked resource. */
  while(flock(FD, LOCK_EX | LOCK_NB) < 0) {
    if(errno == EWOULDBLOCK) {
      printf("Could not get lock.  Sleeping...\n");
      sleep(1);
    } else if(errno == EBADF) {
      printf("ERROR: The descriptor was not valid.\n");
      exit(1);
    } else if(errno == EINVAL) {
      printf("ERROR: The descriptor was not a file.\n");
      exit(1);
    } else if(errno == EOPNOTSUPP) {
      printf("ERROR: The referenced descriptor is not of the correct type.\n");
      exit(1);
    } /* end else/if */
  } /* end while */
  printf("Got the lock.\n");

  /* We ignore possible I/O errors as our goal here is to demo locking, not how to do I/O in a safe way... */
  for(i=0; i<10; i++) {
    printf("Write..");
    write(FD, buf, strlen(buf));
    printf("Sleep..\n");
    sleep(1);
  } /* end for */
    
  close(FD);

  printf("Free up lock.\n");
  flock(FD, LOCK_UN);
} /* end func main */
