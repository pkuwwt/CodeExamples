/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      ptMutex.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Basic pthread programming@EOL
   @Keywords  UNIX pthreads POSIX mutex
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2        
              - Linux (RH 7.3)

   This C program is intended to illustrate various concepts related to POSIX threads (pthreads) including simple mutex and
   condition variable use and basic thread creation and destruction.  Notable things not demonstrated are "Reader/Writer Locking",
   "Semaphores", "Cancellation", and "Scheduling".
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
/* Handy stuff for Multi-Threaded programs (in C only). */
#include "mtUtils.h"

/**********************************************************************************************************************************/
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

/**********************************************************************************************************************************/
void workThread1(void *reqArg);
void workThread2(void *reqArg);

/**********************************************************************************************************************************/
/* This is where it starts. :) */
int main(int argc, char *argv[]) {
  pthread_t thread;
  int *anIntPtr;
  int i;
  int mstrThrdNumber=0;

  mtPrintf("main: startup.\n");

  /* Lock mutex1 -- everyone wants to lock this on startup. */
  mtPrintf("main: locking mutex1.\n");
  if(pthread_mutex_lock(&mutex1)) {
    mtPrintf("ERROR: pthread_mutex_lock() failed.\n");
    exit(1);
  } /* end if */
  mtPrintf("main: mutex1 locked.\n");

  mallocNsetInt(&anIntPtr, ++mstrThrdNumber);
  if(pthread_create(&thread, NULL, (PTHRFUNC*)workThread1, (void *)anIntPtr) != 0) {
    mtPrintf("ERROR: pthread_create() failed.\n");
    exit(1);
  } /* end if */

  mallocNsetInt(&anIntPtr, ++mstrThrdNumber);
  if(pthread_create(&thread, NULL, (PTHRFUNC*)workThread2, (void *)anIntPtr) != 0) {
    mtPrintf("ERROR: pthread_create() failed.\n");
    exit(1);
  } /* end if */

  for(i=0;i<4;i++) {
    mtPrintf("main: sleeping. (%d).\n", i);
    sleep(3);
  } /* end for */

  /* Unlock mutex1 -- everyone waiting to lock this one! */
  mtPrintf("main: unlocking mutex1.\n");
  if(pthread_mutex_unlock(&mutex1)) {
    mtPrintf("ERROR: pthread_mutex_lock() failed.\n");
    exit(1);
  } /* end if */
  mtPrintf("main: mutex1 unlocked.\n");

  for(i=0;i<10;i++) {
    mtPrintf("main: sleeping. (%d).\n", i);
    sleep(3);
  } /* end for */

  mtPrintf("main: shutdown\n");
  return (0);
} /* end func main */

/**********************************************************************************************************************************/
void workThread1(void *reqArg) {

  /* Thread will never be "joined", so we detach it. */
  pthread_detach(pthread_self());

  mtPrintf("worker Type 1 thread %d startup.\n", *(int *)reqArg);

  mtPrintf("worker Type 1 thread %d locking mutex1.\n", *(int *)reqArg);
  if(pthread_mutex_lock(&mutex1)) {
    mtPrintf("ERROR: pthread_mutex_lock() failed.\n");
    exit(1);
  } /* end if */  
  mtPrintf("worker Type 1 thread %d mutex1 locked.\n", *(int *)reqArg);

  mtPrintf("worker Type 1 thread %d unlocking mutex1.\n", *(int *)reqArg);
  if(pthread_mutex_unlock(&mutex1)) {
    mtPrintf("ERROR: pthread_mutex_unlock() failed.\n");
    exit(1);
  } /* end if */  
  mtPrintf("worker Type 1 thread %d mutex1 unlocked.\n", *(int *)reqArg);


  mtPrintf("worker Type 1 thread %d shutdown.\n", *(int *)reqArg);

  /* The argument given to this thread was "malloced", so we free it now. */
  free(reqArg);
} /* end func workerThread1 */

/**********************************************************************************************************************************/
void workThread2(void *reqArg) {
  int tryLockResult;

  /* Thread will never be "joined", so we detach it. */
  pthread_detach(pthread_self());

  mtPrintf("worker Type 2 thread %d startup.\n", *(int *)reqArg);

  mtPrintf("worker Type 2 thread %d locking mutex1.\n", *(int *)reqArg);
  while((tryLockResult = pthread_mutex_trylock(&mutex1))) {
    if(tryLockResult != EBUSY) {
      mtPrintf("ERROR: pthread_mutex_lock() failed.\n");
      exit(1);
    } /* end if */
    mtPrintf("worker Type 2 thread %d still trying to lock mutex1.\n", *(int *)reqArg);

    sleep(1);
  } /* end while */
  mtPrintf("worker Type 2 thread %d mutex1 locked.\n", *(int *)reqArg);

  mtPrintf("worker Type 2 thread %d unlocking mutex1.\n", *(int *)reqArg);
  if(pthread_mutex_unlock(&mutex1)) {
    mtPrintf("ERROR: pthread_mutex_unlock() failed.\n");
    exit(1);
  } /* end if */  
  mtPrintf("worker Type 2 thread %d mutex1 unlocked.\n", *(int *)reqArg);

  mtPrintf("worker Type 2 thread %d shutdown.\n", *(int *)reqArg);

  /* The argument given to this thread was "malloced", so we free it now. */
  free(reqArg);
} /* end func workerThread2 */
