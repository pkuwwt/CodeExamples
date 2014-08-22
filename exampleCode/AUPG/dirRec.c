/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      dirRec.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     Recursive traversal of UNIX directories@EOL
   @Keywords  UNIX directory file traversal
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)              

   This C program is intended to illustrate how to traverse a directory tree recursively -- the traditional way.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <dirent.h>             /* UNIX dirs       POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <utime.h>              /* utime           POSIX */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <time.h>               /* time            C89   */

/**********************************************************************************************************************************/
void travDir(char *dirName);

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  if(argc != 2) {
    printf("ERROR: One argument required.\n");
    exit(1);
  } else {
    travDir(argv[1]);
  } /* end if/else */
  return 0;
} /* end func main */

/**********************************************************************************************************************************/
void travDir(char *dirName) {
  DIR *dp;                 /* The directory pointer. */
  struct dirent *dep;      /* The directory entry. */
  struct stat s;           /* Used to stat each file. */
  char *name;              /* The name files and dirs we find. */
  long len_d_name;         /* holds the length of file names. */
  long len_workingDirName; /* holds the length of the working dir. */

  if((dp = opendir(dirName)) == NULL) {
    printf("ERROR: dirRec: %s: %s\n", dirName, strerror(errno));
  } else {
    len_workingDirName = strlen(dirName);
    while((dep = readdir(dp)) != NULL) {
      if((strcmp(dep->d_name, "..") != 0) && (strcmp(dep->d_name, ".") != 0)) {
        len_d_name = strlen(dep->d_name);
        name = (char *)malloc(len_d_name + len_workingDirName + 2);
        strcpy(name, dirName);
        strcat(name, "/");
        strcat(name, dep->d_name);
        if(lstat(name, &s) < 0) {
          printf("ERROR: dirDepth: %s: %s\n", name, strerror(errno));
          free(name);
        } else {
          if(S_ISDIR(s.st_mode)) {      /* Process directories. */
            printf(" D: %s\n", name);
            travDir(name);
          } else {              /* Process non-directories. */
            printf("ND: %s\n", name);
            free(name);
          } /* end if/else */
        } /* end if/else */
      } /* end if */
    } /* end while */
    closedir(dp);
  } /* end if/else */
} /* end func travDir */
