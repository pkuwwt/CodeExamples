/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      fileData.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     How to get file, and file system, infomation in UNIX.@EOL
   @Keywords  UNIX shell environment variable
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how query and manipulate file meta data like access/modify times, ownership, and
   permissions.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <dirent.h>             /* UNIX dirs       POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <utime.h>              /* utime           POSIX */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <time.h>               /* time            C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct stat s;
  char readlinkBuf[1024];
  int readlinkChCnt;

  /* Make sure we have precicely one argument. */
  if(argc != 2) {
    printf("ERROR: One argument required.\n");
    exit(1);
  } /* end if */

  printf("File name: %s\n", argv[1]);

  /* Stat the file given on the command line. */
  if(lstat(argv[1], &s) < 0) {
    printf("ERROR: fileData");
    exit(2);
  } /* end if */

  printf("File inode number: %lu\n",                (unsigned long)s.st_ino);
  printf("Device number: %lu\n",                    (unsigned long)s.st_dev);
  printf("Device type: %lu\n",                      (unsigned long)s.st_rdev);
  printf("File mode: %lu\n",                        (unsigned long)s.st_mode);
  printf("File owner(UID): %lu\n",                  (unsigned long)s.st_uid);
  printf("File group owner(GID): %lu\n",            (unsigned long)s.st_gid);
  printf("File size: %lu bytes\n",                  (unsigned long)s.st_size);
  printf("File size: %lu blocks\n",                 (unsigned long)s.st_blocks);
  printf("Block size: %lu bytes\n",                 (unsigned long)s.st_blksize);
  printf("Block storage requirements: %lu bytes\n", (unsigned long)(512 * s.st_blocks));
  printf("File atime: %lu\n",                       (unsigned long)s.st_atime);
  printf("File ctime: %lu\n",                       (unsigned long)s.st_ctime);
  printf("File mtime: %lu\n",                       (unsigned long)s.st_mtime);
#ifdef BSD
  printf("File generation number: %lu\n",           (unsigned long)s.st_gen);
#endif

  /* This section is not POSIX!!!  Note the order is important here as
     the different modes are NOT independent bits.  This order
     requirement can be different for different OSes.  This order
     works well for most BSD based systems.*/
  printf("File Type: ");
  if(s.st_mode & S_IFLNK)   printf(" symbolic link\n");
  else if(s.st_mode & S_IFSOCK)  printf(" socket\n");
#ifdef BSD
  else if(s.st_mode & S_IFWHT)   printf(" whiteout\n");
#endif
  else if(s.st_mode & S_IFIFO)   printf(" named pipe (fifo)\n");
  else if(s.st_mode & S_IFCHR)   printf(" character special\n");
  else if(s.st_mode & S_IFDIR)   printf(" directory\n");
  else if(s.st_mode & S_IFBLK)   printf(" block special\n");
  else if(s.st_mode & S_IFREG)   printf(" regular\n");
  else                           printf(" UNKNOWN\n");

  /* This section IS POSIX.  It is safe (note we can't tell if a file
     is a symbolic link or not.  This is a problem. :) */
  if(S_ISDIR(s.st_mode))  printf("Macro check: directory\n");
  if(S_ISCHR(s.st_mode))  printf("Macro check: char special\n");
  if(S_ISBLK(s.st_mode))  printf("Macro check: block special\n");
  if(S_ISREG(s.st_mode))  printf("Macro check: regular file\n");
  if(S_ISFIFO(s.st_mode)) printf("Macro check: fifo or socket\n");

  /* This one is not POSIX!! */
  if(S_ISLNK(s.st_mode))  printf("Macro check: symbolic link\n");

  printf("File Permissions: \n");
  if(s.st_mode & S_ISUID)   printf(" set user id on execution\n");
  if(s.st_mode & S_ISGID)   printf(" set group id on execution\n");
  if(s.st_mode & S_ISVTX)   printf(" save swapped text even after use\n");

  printf("owner permissions: ");
  if(s.st_mode & S_IRUSR)   printf(" read ");
  if(s.st_mode & S_IWUSR)   printf(" write");
  if(s.st_mode & S_IXUSR)   printf(" execute/search");
  printf("\n");

  printf("group permissions: ");
  if(s.st_mode & S_IRGRP)   printf(" read ");
  if(s.st_mode & S_IWGRP)   printf(" write");
  if(s.st_mode & S_IXGRP)   printf(" execute/search");
  printf("\n");

  printf("other permissions: ");
  if(s.st_mode & S_IROTH)   printf(" read ");
  if(s.st_mode & S_IWOTH)   printf(" write");
  if(s.st_mode & S_IXOTH)   printf(" execute/search");
  printf("\n");

  /* Both the macro, S_ISLNK, and the function, readlink, are not POSIX compliant. The readlink function appeared in BSD sometime
     around 4.2. */
  if(S_ISLNK(s.st_mode)) {
    if( (readlinkChCnt = readlink(argv[1], readlinkBuf, 1000)) < 0) {
      printf("ERROR: readlink failure.\n");
      exit(1);
    } else {
      printf("File len: %d\n", readlinkChCnt);
      if(readlinkChCnt >= 0) {
        readlinkBuf[readlinkChCnt] = '\0';
        printf("Link target: %s\n", readlinkBuf);
      } /* end if */
    } /* end if/else */
  } /* end if */

  return 0;
} /* end func main */
