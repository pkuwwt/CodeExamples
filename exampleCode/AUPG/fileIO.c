/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      fileIO.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     UNIX I/O routines@EOL
   @Keywords  UNIX file I/O
   @Std       ISOC POSIX UNIX98
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   Two general approaches are taken for file I/O in C on a UNIX platform: UNIX I/O and ANSI/ISO C I/O.  This program demonstrates
   the UNIX I/O routines.  It is important not to mix ANSI/ISO C I/O with UNIX I/O on the same file.  This will hopelessly confuse
   your program.  Functions exist in the UNIX library to let you do this, but it is best to simply avoid the mess.  On a side note,
   do not mix C++ I/O with UNIX or ANSI/ISO C I/O.  Again, methods exist to make this work, but it is best to simply pick an I/O
   library and stick to it. :)
  
   Most UNIX I/O functions can error with an EINTR error.  In general if this error is seen, the function should be re-attempted.
   This code doesn't demonstrate this.  Also this code doesn't demonstrate the complete behavior of of the write call with respect
   to partial success.  Both of these interesting problems are tackled in the example program called IOerrors.c.  The approach taken
   in this code is typical of file I/O found in many UNIX programs.  The more exotic error handling found in IOerrors.c is normally
   only seen in network programs.
***********************************************************************************************************************************/

#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/uio.h>            /* BSD  I/O        BSD   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <dirent.h>             /* UNIX dirs       POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <fcntl.h>              /* UNIX file ctrl  UNIX  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char buf[255];                /* A character buffer to use for I/O. */
  char *fileName = "foo";
  int returnValue;
  int readNum;
  int FD;

  /* FD stands for File Descriptor.  In ANSI/ISO C I/O one uses a FP or File Pointer.  For UNIX a FD is simply an integer, while
     ANSI/ISO C uses a struct.  ANSI/ISO C restricts a program to 254 open files because each file is uniquely identified by an
     8-bit integer.  The number of open files possible with UNIX I/O varies, but is generally much higher than ANSI/ISO C. */

  /* ************************************************************************** */
  /* First we create/overwrite a file named foo. */

  /* Just as with ANSI/ISO C, we must open the file first.  This can fail.  If errno is EINTR, the open should be retried. */
  if((FD = open(fileName, O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU | S_IRWXG | S_IRWXO)) < 0) {
    perror("ERROR: File open");
    exit(10);
  } /* end if */

  /* Report what file descriptor we got. */
  printf("We opened the file '%s' and got file descriptor number %d\n", fileName, FD);

  strcpy(buf, "This is the first line of the file\n");
  returnValue = write(FD, buf, strlen(buf));    /* How to write a string. */

  /* The return of the write is the number of bytes written.  If it is less than the request, an attempt to write the rest of the
     string should be made.  This function can error because of an interrupt (errno==EINTR).  If an EINTR is encountered, the write
     should be re-attempted. */

  if(returnValue < 0) {
    perror("ERROR: File write");
    exit(11);
  } /* end if */
  printf("The write returned %d\n", returnValue);

  /* Write the first 2 bytes of the integer stored in returnValue.  This won't work if the integer is not at least 2 bytes
     long. :) */
  returnValue = write(FD, &returnValue, 2);
  if(returnValue < 0) {
    perror("ERROR: File write");
    exit(12);
  } /* end if */
  printf("The write returned %d\n", returnValue);

  /* We must close the file to free up system resources.  If we forget to close the file, then it will be closed for us when our
     program exits. */
  if(close(FD) < 0) {           /* Yes, we check for errors. */
    perror("ERROR: File open");
    exit(13);
  } /* end if */

  /* ************************************************************************** */
  /* Now we read the file we just wrote. */

  /* First we open the file.  This can fail.  If errno is EINTR, the open should be retried. */
  if((FD = open(fileName, O_RDONLY)) < 0) {
    perror("ERROR: File open");
    exit(10);
  } /* end if */

  /* Report what file descriptor we got. */
  printf("We opened the file '%s' and got file descriptor number %d\n", fileName, FD);

  /* We now read the data in 1 byte chunks.  Larger reads will be demonstrated later.  read can error with EINTR, and should be
     re-attempted if this error occurs.  read returns 0 at EOF, and -1 upon error. */
  printf("Data in file:\n");
  while((returnValue = read(FD, buf, 1)) > 0) {
    printf("%c", buf[0]);
  } /* end while */

  if(returnValue == 0)
    printf("Finished reading all the data.\n");

  if(returnValue < 0) {
    perror("ERROR: File read");
    exit(11);
  } /* end if */

  /* Now that we have read the file one byte at a time, it is a good idea to see how to read the data more than one byte at a time.
     We could close the file now and reopen it, but instead we demonstrate the seek function.  It is much like the ANSI/ISO C
     function fseek.  We will use this function to "rewind" the filepointer so that we can reread the file from the start.  This
     also demonstrates that reading the EOF marker is not special for UNIX I/O.  Unlike some I/O libraries, reading EOF won't
     automatically close the file.  */

  /* Here it is.  Rewind the file to the start. */
  lseek(FD, 0, SEEK_SET);

  /* We now read the data in 250 byte chunks.  Note that we read the WHOLE file in one step.  It is possible to query the size of
     the file using stat(2), and then to read in most files in one big chunk. */
  readNum = 0;
  printf("Data in file(second read):\n");
  while((returnValue = read(FD, buf, 250)) > 0) {
    buf[returnValue] = '\0';
    readNum++;
    printf("Read #%d: %s", readNum, buf);
  } /* end while */

  if(returnValue == 0)
    printf("Finished reading all the data.\n");

  if(returnValue < 0) {
    perror("ERROR: File read");
    exit(11);
  } /* end if */

  /* Finally we close the file -- after having read it in twice. */
  if(close(FD) < 0) {
    perror("ERROR: File close");
    exit(13);
  } /* end if */

  printf("Normal exit.\n");
  exit(0);
} /* end func main */
