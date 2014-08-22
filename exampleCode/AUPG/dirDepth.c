/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      dirDepth.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     Depth first traversal of UNIX directory trees@EOL
   @Keywords  UNIX readdir depth first directory traversal
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)              

   This C program is intended to illustrate how to traverse a directory tree depth first.  That is to say, that all files at a
   particular level of the tree are stated before the program descends.  It simply prints out the names of all files found.
   
   This program reads all the file names in a directory and places any subdirectory names into a linked list to be traversed later.
   This program has rather elegant implementations using ANSI C++ and the STL instead of the linked lists used here (dirDepth.cpp).
   
   chdir'ing instead of constructing strings might be faster on some systems.  On many systems a memcpy function will operate faster
   than a strcpy/strcat.  The lengths of the strings are computed anyway, so a memcpy is the way to go.
   
   Note that this implementation doesn't suffer from any limitations on the number of open directory descriptors -- this
   implementation only has one open directory descriptor at any time.
   
   The functions used are all POSIX or ISO C++ and thus this program will run on most BSD and SRV implementations of UNIX.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <dirent.h>             /* UNIX dirs       POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <utime.h>              /* utime           POSIX */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <time.h>               /* time            C89   */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
/* This struct is used to hold directory entries that we must traverse later.  We must store them someplace, or else we would have
   to repeatedly rescan the current directory looking for the next directory.  Note the nextNode member.  This is to facilitate the
   construction of a linked list. */
struct myStatStruct {
  char *d_name;
  struct myStatStruct *nextNode;
};

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  DIR *dp;                               /** The directory pointer. */
  struct dirent *dep;                    /** The directory entry. */
  struct stat s;                         /** Used to stat each file. */
  char *name;                            /** Holds the complete path name of the current file. */
  struct myStatStruct *lastDir;          /** Points to the last node in the linked list. */
  struct myStatStruct *workingDir;       /** Points to the current working directory. */
  struct myStatStruct *tmpDir;           /** Used to hold the current workingDir just before deallocation. */
  long len_d_name, len_workingDirName;   /** Integers used to hold the lengths of file names. */

  /* Stat the root of the tree we are to traverse. */
  if((argc == 2) && (lstat(argv[1], &s) < 0)) {
    printf("ERROR: Bad first argument\n");
    exit(1);
  } /* end if */
  /* Now that we have stat'ed the thing, we add it as the first node of our linked list of directories thus initializing our linked
     list. */
  workingDir = (struct myStatStruct *)malloc(sizeof(struct myStatStruct));
  workingDir->d_name = (char *)malloc(strlen(argv[1]) + 1);
  strcpy(workingDir->d_name, argv[1]);
  workingDir->nextNode = NULL;
  lastDir = workingDir;

  while(workingDir != NULL) {
    if((dp = opendir(workingDir->d_name)) == NULL) {
      printf("ERROR: dirDepth: %s: %s\n", workingDir->d_name, strerror(errno));
    } else {
      /* We have opened the directory, now we read each file name from it and store the file names that correspond to sub
         directories in our linked list for traversal later. */
      len_workingDirName = strlen(workingDir->d_name);
      while((dep = readdir(dp)) != NULL) {
        /* Make sure we don't process .. and . */
        if((strcmp(dep->d_name, "..") != 0) && (strcmp(dep->d_name, ".") != 0)) {
          /* Allocate space for and construct the complete path name. */
          len_d_name = strlen(dep->d_name);
          name = (char *)malloc(len_d_name + len_workingDirName + 2);
          strcpy(name, workingDir->d_name);
          strcat(name, "/");
          strcat(name, dep->d_name);
          /* Now that we have a complete path name, let's stat the file in question. */
          if(lstat(name, &s) < 0) {
            printf("ERROR: dirDepth: %s: %s\n", name, strerror(errno));
            free(name);
          } else {
            /* We have successfully stat'ed the file, now let's process it: */
            if(S_ISDIR(s.st_mode)) {    /* Process directories. */
              /* This is a directory, so we must add it to our linked list of directories to traverse. */
              if((lastDir->nextNode = (struct myStatStruct *)malloc(sizeof(struct myStatStruct))) == NULL) {
                printf("ERROR: dirDepth: malloc: %s\n", strerror(errno));
                return 1;
              } /* end if */
              lastDir->nextNode->d_name = name;
              lastDir->nextNode->nextNode = NULL;
              lastDir = lastDir->nextNode;
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
    /* We have finished walking the current directory, so we remove it from the linked list and update the workingDir pointer so
       that it points to the next directory in the list. */
    tmpDir = workingDir;
    workingDir = workingDir->nextNode;
    free(tmpDir->d_name);
    free(tmpDir);
  } /* end while */

  return 0;
} /* end func main */
