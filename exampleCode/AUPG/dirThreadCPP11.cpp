// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      dirThreadCPP11.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Threaded traversal of UNIX directory trees@EOL
   @Keywords  UNIX readdir readdir_r threaded directory traversal
   @Std       ISOC++11 POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Debian 7.4 + gcc (Debian 4.7.2-5) 4.7.2
              - Ubuntu 10.10 + gcc 4.4.4-14ubuntu5
              - RHEL 4.8 + gcc 4.5.1              

   This program is a C++11 version of dirThreadCPP98.cpp.  C++11 features replace POSIX ones for threading and timing.  Unlike the
   CPP98 version, this code is self contained and has a few C++ stylistic changes so as to render the code a bit less offensive to
   C++ wizards. :)
***********************************************************************************************************************************/

#include <chrono>                  /* time                    C++11    */
#include <thread>                  /* threads                 C++11    */
#include <mutex>
#include <condition_variable>
#include <list>                    /* STL list                C++98/11 */
#include <vector>                  /* STL vector              C++98/11 */ 
#include <algorithm>               /* STL algorithm           C++98/11 */
#include <iostream>                /* C++ iostream            C++98/11 */
#include <cstring>                 /* C Strings std::         C++98/11 */
#include <errno.h>                 /* error stf               POSIX    */
#include <dirent.h>                /* UNIX dirs               POSIX    */
#include <sys/stat.h>              /* UNIX stat               POSIX    */
#include <sys/types.h>             /* UNIX types              POSIX    */
#include <unistd.h>                /* UNIX std stf            POSIX    */

/**********************************************************************************************************************************/
/** Use the absolutely thread-safe readdir_r function, or use the much faster, mostly thread-safe readdir system call */
#define SAFEREADDIR 1

/**********************************************************************************************************************************/
/** The list of dirs to traverse and related access control variables. */
std::list<char *>        gblDirList;
std::mutex               dirList_mutex;
std::condition_variable  dirList_cond;

/**********************************************************************************************************************************/
/** Number of threads with nothing to do. We protect this variable with the same mutex we use for the global directory list. */
int numThreadsWaiting;

/**********************************************************************************************************************************/
/** Protect screen prints */
std::mutex console_mutex;

/**********************************************************************************************************************************/
/** Designed to be used by a thread. */
void readDir(char *workingDir);

/**********************************************************************************************************************************/
/** Worker thread function */ 
void workerThread();

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct stat s;
  char *wrkBuf;
  const int numThreads=55;
  std::thread threadList[numThreads];

  /* Stat the root of the tree we are to traverse. */
  if((argc == 2) && (lstat(argv[1], &s) < 0)) {
    { std::lock_guard<std::mutex> console_lock(console_mutex);
      std::cout << "ERROR: Bad first argument" << std::endl;
    } /* end lock console_lock */
    exit(1);
  } /* end if */
  /* Now that we have stat'ed' the thing, we add it as the list of directories */
  wrkBuf = (char *)malloc(strlen(argv[1]) + 1);
  strcpy(wrkBuf, argv[1]);
  gblDirList.push_back(wrkBuf);
 
  /* Here we fire off n-threads */
  { std::lock_guard<std::mutex> dirList_lock(dirList_mutex);
    numThreadsWaiting = 0;
    for(int i = 0; i < numThreads; i++) {
      threadList[i] = std::thread(workerThread);
      (threadList[i]).detach();
    } /* end for */
  } /* end lock dirList */

  /* Loop until we have nothing left to do */
  int mustWait = 1;
  while(mustWait) {
    { std::lock_guard<std::mutex> dirList_lock(dirList_mutex);
      if((gblDirList.empty()) && (numThreadsWaiting == numThreads)) {
        mustWait = 0;
      } /* end if */
    } /* end lock dirList */
    /* We broadcast here to avoid deadlock. */
    dirList_cond.notify_all();
    /* We sleep for a bit to reduce broadcast frequency. */
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  } /* end while */
  dirList_mutex.unlock();

  /* Everything is done now */
  return 0;
} /* end func main */

/**********************************************************************************************************************************/
void workerThread() {
  char *workingDir;

  /* Just set here until the master thread unlocks dirList_mutex! */
  { std::lock_guard<std::mutex> dirList_lock(dirList_mutex);
    ;
  } /* end lock dirList */

  while(1) {
    std::unique_lock<std::mutex> dirList_lock(dirList_mutex);
    if(gblDirList.empty()) {
      numThreadsWaiting++;
      while(gblDirList.empty()) {
        dirList_cond.wait(dirList_lock);
      } /* end while */
      /* If we get here, we have dirList_mutex locked. */
      numThreadsWaiting--;
      dirList_lock.unlock();
    } else {
      workingDir = gblDirList.back();
      gblDirList.pop_back();
      dirList_lock.unlock();
      readDir(workingDir);
    } /* end if/else */
  } /* end while */
} /* end func workerThread */

/**********************************************************************************************************************************/
void readDir(char *workingDir) {
  const int UPLOADTRIG=10;
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
    { std::lock_guard<std::mutex> console_lock(console_mutex);
      std::cout << "ERROR(4): dirThreadCPP11: opendir: " << workingDir << ": Undetermined" << std::endl;
    } /* end lock console_lock */
  } else {
    int name_max=pathconf(workingDir, _PC_NAME_MAX);
    if(name_max <= 0) {
      { std::lock_guard<std::mutex> console_lock(console_mutex);
        std::cout << "ERROR(7): dirThreadCPP11: pathconf: " << workingDir << ": Undetermined" << std::endl;
      } /* end lock console_lock */
      exit(1);
    } /* end if */
#if SAFEREADDIR
    /* Some platforms have large dirent sizes, but most systems have lots of RAM... */
    if((dep = (struct dirent *)malloc(sizeof(struct dirent))) == NULL) {
    { std::lock_guard<std::mutex> console_lock(console_mutex);
      std::cout << "ERROR(8): dirThreadCPP11: malloc: direntBlob: Undetermined" << std::endl;
    } /* end lock console_lock */
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
            { std::lock_guard<std::mutex> console_lock(console_mutex);
              std::cout << "ERROR(5): dirThreadCPP11: malloc: Undetermined" << std::endl;
            } /* end lock console_lock */
            exit(1);
          } /* end if */
          strcpy(name, workingDir);
          strcat(name, "/");
          strcat(name, dep->d_name);
          if(lstat(name, &s)) {
            { std::lock_guard<std::mutex> console_lock(console_mutex);
              std::cout << "ERROR(6): dirThreadCPP11: lstat: " << name << ": Undetermined" << std::endl;
            } /* end lock console_lock */
            free(name);
          } else {
            if(S_ISDIR(s.st_mode)) {
              localDirList.push_back(name);
              localDirListLen++;
              { std::lock_guard<std::mutex> console_lock(console_mutex);
                std::cout << " D: " << name << std::endl;
              } /* end lock console_lock */
              /* If we found UPLOADTRIG directories, then put them in the global list. We don't use localDirList.size() as it is
                 O(n) with some implementations -- which is OK per the C++ standard. */
              if(UPLOADTRIG && (localDirListLen > UPLOADTRIG)) {
                { std::lock_guard<std::mutex> dirList_lock(dirList_mutex);
                  gblDirList.splice(gblDirList.end(), localDirList);
                  dirList_cond.notify_all();
                } /* end lock dirList */
              } /* end if */
            } else {
              { std::lock_guard<std::mutex> console_lock(console_mutex);
                std::cout << "ND: " << name << std::endl;
              } /* end lock console_lock */
              free(name);
            } /* end if/else */
          } /* end if/else */
        } /* end if */
      } /* end if */
    } /* end while */
    closedir(dp);
    free(dep);
  } /* end if/else */
  { std::lock_guard<std::mutex> dirList_lock(dirList_mutex);
    gblDirList.splice(gblDirList.end(), localDirList);
    dirList_cond.notify_all();
  } /* end lock dirList */
  /* Free up the string for this dirent. */
  free(workingDir);
} /* end func readDir */
