// -*- Mode:C++; Coding:us-ascii-unix; fill-column:132 -*-
/**********************************************************************************************************************************/
/**
   @file      dirFastC.cpp
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     traversal of UNIX directory trees@EOL
   @Keywords  UNIX readdir directory traversal
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C++ program is intended to illustrate how to traverse a directory with only one directory open at a time and with a fast
   data structure.  The traversal is not depth first.
   
   This program reads all the file names in a directory and places any subdirectory names into an STL vector to be traversed later.
   A C version of this program uses a home grown linked list (dirDepth.c).
   
   The string handling is C-style, as that is what natively works with the UNIX C APIs.  chdir'ing instead of constructing strings
   might be faster on some systems.  On many systems a memcpy function will operate faster than a strcpy/strcat.  The lengths of the
   strings are computed anyway, so a memcpy is the way to go.
   
   The functions used are all POSIX or ISO C++ and thus this program will run on most BSD and SRV implementations of UNIX.
***********************************************************************************************************************************/

#include <vector>                  /* STL vector              C++98/11 */ 
#include <stdio.h>                 /* I/O lib                 C89      */
#include <string.h>                /* Strings                 C89      */
#include <sys/types.h>             /* UNIX types              POSIX    */
#include <dirent.h>                /* UNIX dirs               POSIX    */
#include <errno.h>                 /* error stf               POSIX    */
#include <utime.h>                 /* utime                   POSIX    */
#include <sys/stat.h>              /* UNIX stat               POSIX    */
#include <time.h>                  /* time                    C89      */
#include <stdlib.h>                /* Standard Lib            C89      */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  DIR *dp;                               /** The directory pointer. */
  struct dirent *dep;                    /** The directory entry. */
  struct stat s;                         /** Used to stat each file. */
  char *name;                            /** Holds the complete path name of the current file. */
  char *wrkBuf;
  char *curDir;
  long len_d_name, len_curDir;   /** Integers used to hold the lengths of file names. */
  std::vector<char *> listOdirs;

  /* Stat the root of the tree we are to traverse. */
  if((argc == 2) && (lstat(argv[1], &s) < 0)) {
    printf("ERROR: Bad first argument\n");
    exit(1);
  }
  /* Now that we have stat'ed the thing, we add it as the list of directories */
  wrkBuf = (char *)malloc(strlen(argv[1]) + 1);
  strcpy(wrkBuf, argv[1]);
  listOdirs.push_back(wrkBuf);

  while(!(listOdirs.empty())) {
    curDir = listOdirs.back();
    listOdirs.pop_back();
    if((dp = opendir(curDir)) == NULL) {
      printf("ERROR: dirDepth: %s: %s\n", curDir, strerror(errno));
    } else {
      /* We have opened the directory, now we read each file name from it and store the file names that correspond to sub
         directories in our linked list for traversal later. */
      len_curDir = strlen(curDir);
      while((dep = readdir(dp)) != NULL) {
        /* Make sure we don't process .. and . */
        if((strcmp(dep->d_name, "..") != 0) && (strcmp(dep->d_name, ".") != 0)) {
          /* Allocate space for and construct the complete path name. */
          len_d_name = strlen(dep->d_name);
          name = (char *)malloc(len_d_name + len_curDir + 2);
          strcpy(name, curDir);
          strcat(name, "/");
          strcat(name, dep->d_name);
          /* Now that we have a complete path name, let's stat the file in question. */
          if(lstat(name, &s) < 0) {
            printf("ERROR: dirDepth: %s: %s\n", name, strerror(errno));
            free(name);
          } else {
            /* We have successfully stated the file, now let's process it: */
            if(S_ISDIR(s.st_mode)) {    /* Process directories. */
              /* This is a directory, so we must add it to our list of directories to traverse. */
              listOdirs.push_back(name);
              printf(" D: %s\n", name);
            } else {            /* Process non-directories. */
              printf("ND: %s\n", name);
              /* Free up the space for the pathname. */
              free(name);
            } /* end if/else */
          } /* end if/else */
        } /* end if */
      } /* end while */
      closedir(dp);
    } /* end if/else */
    /* We have finished walking the current directory, so we remove it from the list. */
    free(curDir);
  } /* end while */
  return 0;
} /* end func main */
