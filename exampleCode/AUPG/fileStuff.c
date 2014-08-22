/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      fileStuff.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     Recursive traversal of UNIX directories@EOL
   @Keywords  UNIX directory file traversal
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how to perform several file operations using both UNIX and ISO C APIs.
   Functions demonstrated include:

      - chdir
      - chmod
      - link
      - rename
      - rmdir
      - symlink
      - mkdir
      - unlink
      - remove
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
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <fcntl.h>              /* UNIX file ctrl  UNIX  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int fd;

  /* create a directory */
  if(mkdir("foo", S_IRWXU)) {
    perror("mkdir() failure");
    exit(1);
  } /* end if */

  /* change the mode of the new directory.  See 'fchmod' for how to do this with a file descriptor*/
  if(chmod("foo", S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH)) {
    perror("chmod() failure");
    exit(2);
  } /* end if */

  /* change into new directory */
  if(chdir("foo")) {
    perror("chdir() failure");
    exit(2);
  } /* end if */

  /* create a new file named 'bar' */
  if((fd=open("bar", O_CREAT | O_TRUNC | O_WRONLY, S_IRWXU | S_IRWXG | S_IRWXO)) < 0) {
    perror("open() failure");
    exit(2);
  } /* end if */
  write(fd, "HELLO\n\n", 7);
  if(close(fd)) {
    perror(NULL);
    exit(2);
  } /* end if */

  /* create a new file named 'barben' */
  if((fd=open("barben", O_CREAT | O_TRUNC | O_WRONLY, S_IRWXU | S_IRWXG | S_IRWXO)) < 0) {
    perror("open() failure");
    exit(2);
  } /* end if */
  write(fd, "HELLO DUDE\n\n", 12);
  if(close(fd)) {
    perror(NULL);
    exit(2);
  } /* end if */

  /* rename 'barben' to 'barber' the ISO C way. The UNIX way to do this is with 'link' and 'unlink'. */
  if(rename("barben", "barber")) {
    perror("rename() failure");
    exit(2);
  } /* end if */

  /* create a symlink to 'bar' named 'babar' */
  if(symlink("bar", "babar")) {
    perror("symlink() failure");
    exit(2);
  } /* end if */

  /* create a hard link to 'bar' named 'harbar' */
  if(link("bar", "harbar")) {
    perror("link() failure");
    exit(2);
  } /* end if */

  /* Wait around so that the user can explore what was created. */
  printf("Hit [enter] to continue...\n");
  getchar();

  /* The UNIX way to "delete" a file */
  if(unlink("harbar")) {
    perror("unlink() failure");
    exit(2);
  } /* end if */

  /* The ISO C way to delete a file. */
  if(remove("babar")) {
    perror("remove() failure");
    exit(2);
  } /* end if */

  /* Nuke the rest of the files we created. */
  if(remove("bar")) {
    perror("remove() failure");
    exit(2);
  } /* end if */
  if(remove("barber")) {
    perror("remove() failure");
    exit(2);
  } /* end if */

  /* change back down one directory */
  if(chdir("../")) {
    perror("chdir() failure");
    exit(2);
  } /* end if */

  /* Remove the directory --- it is empty now. */
  if(rmdir("foo")) {
    perror("rmdir() failure");
    exit(2);
  } /* end if */

  return 0;
} /* end func main */
