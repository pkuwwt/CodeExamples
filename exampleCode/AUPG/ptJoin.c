/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      ptJoin.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Basic pthread programming@EOL
   @Keywords  UNIX pthreads POSIX mutex
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2        
              - Linux (RH 7.3)

   This little C program illustrates the pthread_join() system call that is part of the POSIX threads API.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <dirent.h>             /* UNIX dirs       POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <utime.h>              /* utime           POSIX */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <time.h>               /* time            C89   */
#include <pthread.h>            /* threads         POSIX */
#include <sched.h>              /* threads         POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */

/**********************************************************************************************************************************/
#include "mtUtils.h"

/**********************************************************************************************************************************/
void workThread1(void *reqArg);
void workThread2(void *reqArg);

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  pthread_t thread;
  void *threadReturn;

  mtPrintf("main: startup...\n");

  if(pthread_create(&thread, NULL, (PTHRFUNC*)workThread1, (void *)NULL) != 0) {
       mtPrintf("ERROR: pthread_create() failed.\n");
       exit(1);
     } /* end if */
  mtPrintf("main: waiting for thread 1 shutdown...\n");
  /* Wait, don't care about the exit value... */
  if(pthread_join(thread, NULL)) {
    mtPrintf("ERROR: pthread_join() failed.\n");
    exit(1);
  } /* end if */

  if(pthread_create(&thread, NULL, (PTHRFUNC*)workThread2, (void *)NULL) != 0) {
       mtPrintf("ERROR: pthread_create() failed.\n");
       exit(1);
     } /* end if */
  mtPrintf("main: waiting for thread shutdown...\n");
  /* Wait, don't care about the exit value... */
  if(pthread_join(thread, &threadReturn)) {
       mtPrintf("ERROR: pthread_join() failed.\n");
       exit(1);
  } /* end if */
  /* We know that the thread returned an int, so we cast-n-dereference threadReturn to print the result. */
  mtPrintf("main: Thread ended with exit code: %d\n", *((int*)threadReturn));
  /* We know that the thread malloced space for threadReturn, so we free it. */
  free(threadReturn);

  mtPrintf("main: Goodbye.\n");
  return (0);
} /* end func main */

/**********************************************************************************************************************************/
void workThread1(void *reqArg) {
  int i;

  mtPrintf("thread 1 startup\n");
  for(i=0;i<3;i++) {
    sleep(1);
    mtPrintf("thread 1 waiting...%d.\n", i);
  } /* end for */
  mtPrintf("thread 1 shutdown.\n");

} /* end func workerThread1 */

/**********************************************************************************************************************************/
void workThread2(void *reqArg) {
  int i, *exitValue;

  mtPrintf("thread 2 startup\n");
  for(i=0;i<3;i++) {
    sleep(1);
    mtPrintf("thread 2 waiting...%d.\n", i);
  } /* end for */
  mtPrintf("thread 2 shutdown.\n");
  mallocNsetInt(&exitValue, 42);
  pthread_exit(exitValue);

} /* end func workerThread2 */
