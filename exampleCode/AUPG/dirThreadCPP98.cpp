// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      dirThreadCPP98.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Threaded traversal of UNIX directory trees@EOL
   @Keywords  UNIX readdir readdir_r threaded directory traversal
   @Std       ISOC++98 POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - SPARC Solaris 7, 8
              - x86_64 Solaris 10
              - MacOS X.2, X.4, X.5, X.6
              - RedHat 7.3
              - RHEL 4.6, 4.8
              - SLED 10.2

   This C++ program is intended to illustrate how to traverse a directory tree using a high performance threaded approach.  It
   simply prints out the names of all files found.

   The performance boost comes because this code can multiplex several I/O calls.  The resulting speed boost is often 50x for
   network file systems.
   
   In operation, this program is rather similar to dirDepthC.cpp in that it reads all the file names in a directory and places
   sub-directory names into an STL container to be traversed later.  Unlike dirDepthC.cpp this method provides an unknown ordering
   for the traversal both because we pull off the last item on the list first and because directories will be traversed by diffrent
   threads.

   A note on SAFEREADDIR: readir() uses a static structure to return results, but the POSIX standard requires that it use a
   DIFFERENT static structure per directory handle.  Thus readdir() COULD be POSIX compliant and still thread safe in our
   application -- no two threads ever read the same directory.  Unfortunately the standard expressly allows readdir to be non-thread
   safe.  For example, it could use some other hidden global state across calls and still be in spec.  In practice most God fearing
   UNIX-like OSes don't do this, and thus have a perfectly safe readdir() for this application; however, if someone decides to use
   this code on a heathen UNIX-like OS we provide the option to use readdir_r via SAFEREADDIR and accept the extra code, malloc, and
   performance (lock) penalty.  Note that many systems have a lock around the innards of readdir_r that allow the process to only
   run one at a time, and that will dramatically slow things down when SAFEREADDIR is 1.
   
   The string handling is C-style, as the code is heavily laden with UNIX C API calls, and they all use old school, null terminated,
   C-strings.  While everything is a C-string, we don't always use the string functions strcpy() and strcat() because memcpy()
   operates faster on most systems.  The lengths of the strings are known or must be computed anyhow, so memcpy() is a good choice.
   
   Note that this implementation doesn't suffer from limitations on the number of open directory descriptors like recursive
   approaches frequently do.

   The functions used are all POSIX or ISO C++ and thus this program will run on most UNIX-like OSes.
***********************************************************************************************************************************/

#include <list>                    /* STL list                C++98/11 */
#include <sys/types.h>             /* UNIX types              POSIX    */
#include <stdio.h>                 /* I/O lib                 C89      */
#include <string.h>                /* Strings                 C89      */
#include <dirent.h>                /* UNIX dirs               POSIX    */
#include <errno.h>                 /* error stf               POSIX    */
#include <utime.h>                 /* utime                   POSIX    */
#include <sys/stat.h>              /* UNIX stat               POSIX    */
#include <time.h>                  /* time                    C89      */
#include <pthread.h>               /* threads                 POSIX    */
#include <sched.h>                 /* threads                 POSIX    */
#include <stdlib.h>                /* Standard Lib            C89      */
#include <unistd.h>                /* UNIX std stf            POSIX    */

/**********************************************************************************************************************************/
#include "mtUtils.h"

/**********************************************************************************************************************************/
/** Maximum number-1 of threads to run at one time. */
#define NUMTHREADS 55

/** Number of directories to read before updating global linked list.  Set to 0 for adaptive maximum.*/
#define UPLOADTRIG 10

/** Use the absolutely thread-safe readdir_r function, or use the much faster, mostly thread-safe readdir system call */
#define SAFEREADDIR 0

/**********************************************************************************************************************************/
/** The list of dirs to traverse. */
std::list<char *> gblDirList;

/**********************************************************************************************************************************/
/** Controls access to gblDirList */
pthread_mutex_t dirList_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t dirList_cond = PTHREAD_COND_INITIALIZER;

/**********************************************************************************************************************************/
/** Designed to be used by a thread. */
void readDir(char *workingDir);

/**********************************************************************************************************************************/
/** This is a worker thread function that is intended to run readDir as a function. It must have extern C linkage as that is
    part of the POSIX threads standard. */ 
extern "C" { void workerThread(void *reqArg); }

/**********************************************************************************************************************************/
/**  Useful typedef for a POSIX thread function.  Note we put it in an extern block to avoid warnings about invalid types on
     some compilers (Sun Compilers). */
extern "C" { typedef void*  PTHRFUNC(void*);  }

/**********************************************************************************************************************************/
/** Number of threads with nothing to do. We protect this variable with the same mutex we use for the global directory list. */
int numThreadsWaiting;

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct stat s;
  char *wrkBuf;
  pthread_t thread[NUMTHREADS];
  int mustWait = 1, i;

  /* Stat the root of the tree we are to traverse. */
  if((argc == 2) && (lstat(argv[1], &s) < 0)) {
    mtPrintf("ERROR: Bad first argument\n");
    exit(1);
  } /* end if */
  /* Now that we have stat'ed' the thing, we add it as the list of directories */
  wrkBuf = (char *)malloc(strlen(argv[1]) + 1);
  strcpy(wrkBuf, argv[1]);
  gblDirList.push_back(wrkBuf);
 
  /* Lock the dirList_mutex to keep new threads from going right away. */
  pthread_mutex_lock(&dirList_mutex);

  /* Here we fire off n-threads */
  numThreadsWaiting = 0;
  for(i = 0; i < NUMTHREADS; i++) {
    if(pthread_create(&(thread[i]), NULL, (PTHRFUNC*)workerThread, (void *)NULL) != 0) {
      mtPrintf("ERROR(9): dirThreadCPP98: pthread_create: Couldn't create thread.\n");
      exit(1);
    } /* end if */
    pthread_detach(thread[i]);
  } /* end for */

  /* Unlock dirList_mutex to unleash all the threads..*/
  pthread_mutex_unlock(&dirList_mutex);

  /* Loop until we have nothing left to do */
  mustWait = 1;
  while(mustWait) {
    pthread_mutex_lock(&dirList_mutex);
    if((gblDirList.empty()) && (numThreadsWaiting == NUMTHREADS)) {
      mustWait = 0;
    } /* end if */
    pthread_mutex_unlock(&dirList_mutex);

    /* We broadcast here to avoid deadlock. */
    pthread_cond_broadcast(&dirList_cond);
    /* We sleep for a bit to reduce broadcast frequency. */
    struct timespec nanotime;
    nanotime.tv_sec = 0;
    nanotime.tv_nsec = 1000;
    nanosleep(&nanotime, NULL);

  } /* end while */
  pthread_mutex_unlock(&dirList_mutex);

  /* On some platforms we get stuck unless we kill off the threads */
  for(i = 0; i < NUMTHREADS; i++) {
    pthread_cancel(thread[i]);
  } /* end for */

  /* Everything is done now */
  return 0;
} /* end func main */

/**********************************************************************************************************************************/
void workerThread(void *reqArg) {
  char *workingDir;

  /* Just set here until the master thread unlocks dirList_mutex! */
  pthread_mutex_lock(&dirList_mutex);
  pthread_mutex_unlock(&dirList_mutex);

  while(1) {
    pthread_mutex_lock(&dirList_mutex);
    if(gblDirList.empty()) {
      numThreadsWaiting++;
      while(gblDirList.empty()) {
        pthread_cond_wait(&dirList_cond, &dirList_mutex);
      } /* end while */
      /* If we get here, we have dirList_mutex locked. */
      numThreadsWaiting--;
      pthread_mutex_unlock(&dirList_mutex);
    } else {
      workingDir = gblDirList.back();
      gblDirList.pop_back();
      pthread_mutex_unlock(&dirList_mutex);
      readDir(workingDir);
    } /* end if/else */
  } /* end while */
} /* end func workerThread */

/**********************************************************************************************************************************/
void readDir(char *workingDir) {
  DIR *dp;
  struct dirent *dep;
#if SAFEREADDIR
  struct dirent *depp;
  int readdirRes;
#endif
  struct stat s;
  char *name;                             /* Working space for string manipulation */
  long len_d_name, len_workingDirName;    /* String lengths */
  int haveMore;
  int localDirListLen=0;
  std::list<char *> localDirList;        /* Local working list of directories */

  if( (dp = opendir(workingDir)) == NULL) {
    mtPrintf("ERROR(4): dirThreadCPP98: opendir: %s: Undetermined\n", workingDir);
  } else {
    int name_max=pathconf(workingDir, _PC_NAME_MAX);
    if(name_max <= 0) {
      mtPrintf("ERROR(7): dirThreadCPP98: pathconf: %s: Undetermined\n", workingDir);
      exit(1);
    } /* end if */
#if SAFEREADDIR
    /* Some platforms have large dirent sizes, but most systems have lots of RAM... */
    if((dep = (struct dirent *)malloc(sizeof(struct dirent))) == NULL) {
      mtPrintf("ERROR(8): dirThreadCPP98: malloc: direntBlob: Undetermined\n");
      exit(1);
    } /* end if */
#endif
    len_workingDirName = strlen(workingDir);
    haveMore = 1;
    while(haveMore) {
#if SAFEREADDIR
      readdirRes = readdir_r(dp, dep, &depp);
#else
      dep = readdir(dp);
#endif
      if(
#if SAFEREADDIR
        /* Note that we should do something different for errors for readdir_r... */
        ((depp == NULL) || (readdirRes != 0))
#else
        /* Note that we should check errno for readdir as it might not be EOF but an error... */
        (dep == NULL)
#endif
        ) {
        haveMore = 0;
      } else {
        if((strcmp(dep->d_name, "..") != 0) && (strcmp(dep->d_name, ".") != 0)) {
          len_d_name = strlen(dep->d_name);
          if((name = (char *)malloc(len_d_name + len_workingDirName + 2)) == NULL) {
            mtPrintf("ERROR(5): dirThreadCPP98: malloc: Undetermined\n");
            exit(1);
          } /* end if */
          strcpy(name, workingDir);
          strcat(name, "/");
          strcat(name, dep->d_name);
          if(lstat(name, &s)) {
            mtPrintf("ERROR(6): dirThreadCPP98: lstat: %s: Undetermined\n", name);
            free(name);
          } else {
            if(S_ISDIR(s.st_mode)) {
              localDirList.push_back(name);
              localDirListLen++;
              mtPrintf(" D: %s\n", name);
              /* If we found UPLOADTRIG directories, then put them in the global list. We don't use localDirList.size() as it
                 might be O(1) or O(n) per the C++ standard. */
              if(UPLOADTRIG && (localDirListLen > UPLOADTRIG)) {
                pthread_mutex_lock(&dirList_mutex);
                gblDirList.splice(gblDirList.end(), localDirList);
                pthread_cond_broadcast(&dirList_cond);
                pthread_mutex_unlock(&dirList_mutex);
              } /* end if */
            } else {
               mtPrintf("ND: %s\n", name);
              free(name);
            } /* end if/else */
          } /* end if/else */
        } /* end if */
      } /* end if */
    } /* end while */
    closedir(dp);
    free(dep);
  } /* end if/else */
  pthread_mutex_lock(&dirList_mutex);
  gblDirList.splice(gblDirList.end(), localDirList);

  pthread_cond_broadcast(&dirList_cond);
  pthread_mutex_unlock(&dirList_mutex);
  /* Free up the string for this dirent. */
  free(workingDir);
} /* end func readDir */
