/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      ptCond.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Basic pthread programming@EOL
   @Keywords  UNIX pthreads POSIX mutex
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2        
              - Linux (RH 7.3)

   This C program is intended to illustrate simple use of condition variables in the POSIX threads API.  This code also demonstrates
   how a thread may detach itself.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
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
pthread_mutex_t the_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  the_cond  = PTHREAD_COND_INITIALIZER;

/**********************************************************************************************************************************/
void workThread(void *reqArg);

/**********************************************************************************************************************************/
/* This is where it starts. :) */
int main(int argc, char *argv[]) {
  pthread_t thread;
  int *anIntPtr;
  int i;
  int mstrThrdNumber=0;

  mtPrintf("main: starting up.\n");

  for(i=0; i<4; i++) {
  /* We create a new int for each thread we create... */
  mallocNsetInt(&anIntPtr, ++mstrThrdNumber);
  if(pthread_create(&thread, NULL, (PTHRFUNC*)workThread, (void *)anIntPtr) != 0) {
       mtPrintf("ERROR: pthread_create() failed.\n");
       exit(1);
     } /* end if */
  } /* end for */

  for(i=0; i<4; i++) {
    mtPrintf("main: doing nothing. (%d)\n", i);
    sleep(1);
  } /* end for */

  mtPrintf("main: broadcasting.\n", i);

  if(pthread_mutex_lock(&the_mutex)) {
    mtPrintf("ERROR: pthread_mutex_lock() failed.\n");
    exit(1);
  } /* end if */

  if(pthread_cond_broadcast(&the_cond)) {
    mtPrintf("ERROR: pthread_cond_broadcast() failed.\n");
    exit(1);
  } /* end if */

  /* Replace: pthread_cond_broadcast(&the_cond);
     with:    pthread_cond_signal(&the_cond);
     and see what happens.  Only ONE thread will wake up.  Use signal
     to wake up just one thread, and broadcast to wake them all up. */

  for(i=0; i<4; i++) {
    mtPrintf("main:  doing nothing (still have the lock). (%d)\n", i);
    sleep(1);
  } /* end for */

  mtPrintf("main: Unlock.\n", i);
  if(pthread_mutex_unlock(&the_mutex)) {
    mtPrintf("ERROR: pthread_mutex_unlock() failed.\n");
    exit(1);
  } /* end if */

  for(i=0; i<8; i++) {
    mtPrintf("main:  doing nothing (no lock lock).. (%d)\n", i);
    sleep(1);
  } /* end for */

  return (0);
} /* end func main */

/**********************************************************************************************************************************/
void workThread(void *reqArg) {

  /* We detach the thread as it will never be joined. */
  pthread_detach(pthread_self());

  mtPrintf("thread %i startup.\n", *(int *)reqArg);

  /* Get the lock  */
  if(pthread_mutex_lock(&the_mutex)) {
    mtPrintf("ERROR: pthread_mutex_unlock() failed.\n");
    exit(1);
  } /* end if */

  mtPrintf("thread (%d) got 1st lock, gonna wait now...\n", *(int *)reqArg);

  /* Wait on the cond */
  if(pthread_cond_wait(&the_cond, &the_mutex)) {
    mtPrintf("ERROR: pthread_cond_wait() failed.\n");
    exit(1);
  } /* end if */
  mtPrintf("thread (%d) wakeup..\n", *(int *)reqArg);

  /* NOTE: We have the_mutex LOCKED at this point.  After waiting for a condition variable, you always get the lock after the
     function returns. */

  /* unlock the mutex associated with the condition variable. */
  mtPrintf("thread (%d) unlock..\n", *(int *)reqArg);
  if(pthread_mutex_unlock(&the_mutex)) {
    mtPrintf("ERROR: pthread_mutex_unlock() failed.\n");
    exit(1);
  } /* end if */

  /* The argument given to this thread was "malloced" just for this thread, so we free it now. */
  free(reqArg);
} /* end func workerThread */
