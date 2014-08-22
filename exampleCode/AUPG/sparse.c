/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      sparse.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     UNIX sparse files@EOL
   @Keywords  UFS filesystem sparse hole file
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - BROKEN: Linux (RH 7.3)
              - BROKEN: MacOS X.2

   This C program is intended to illustrate how one can create a file with "holes" in it.  A holy file in UNIX is a file that is
   larger, logically speaking, than the space it takes up.  This can happen if a file is open, and one seeks past the end of file,
   and writes to the file.  Many people think of this as a UNIX thing, but in reality it has more to do with the UFS filesystem many
   UNIX variants use.  For example, do NOT try to run this program on a MacOS X system with an HFS+ filesystem.  While MacOS X is
   BSD based, the filesystem it most often uses is HFS+.  HFS+ requires that the "empty space" be filled in with zeros that are
   actually written to the disk.  So if you try this program in such an environment, you will end up with a full disk --- unless you
   have 10Tb of free disk space.

   This must be built as 64-bit on some OSES (Solaris Ex: cc -xarch=v9)
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <fcntl.h>              /* UNIX file ctrl  UNIX  */
#include <stdio.h>              /* I/O lib         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int fd;

  fd = creat("s", O_WRONLY);
  /* First we write a byte at the start of the file.  This is not required to make a sparse file. */
  write(fd, "x", 5);
  /* Now we seek way past the end of the file. Note that we use an unsigned integer constant to avoid overflow. */
  lseek(fd, 1024 * 1024UL * 1024 * 1024, SEEK_SET);
  /* Now we write another byte to the end of the file.  We have now created a 10Tb file. */
  write(fd, "x", 5);
  close(fd);

  return 0;
} /* end func main */
