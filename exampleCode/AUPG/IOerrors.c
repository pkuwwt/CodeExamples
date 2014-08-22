/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      IOerrors.c
   @author    Mitch Richling@Mee
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     Example program for UNIX I/O and EINTR handling@EOL
   @Keywords  UNIX example file I/O EINTR
   @Std       C89
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how to handle the interrupt error (EINTR) returns possible with many I/O functions.  In
   general an EINTR error is not an error to exit over, and the I/O operation should be retried.  This code also demonstrates how to
   handle write calls that are only partly successful.  This kind of code is not generally required for file I/O, but is the norm
   for network programs.

   If one is determined to take care of EINTR errors correctly, then it is best to wrap the I/O functions inside of user defined
   functions that take care of the error conditions in order to reduce the code size and complexity.  I have chosen to take care of
   the EINTR errors in-line with no added functions because this program only calls each I/O function one time!
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
  int FD;
  char *fileName = "foo";       /* The filename we are going to work with. */
  int returnValue;
  const int maxEINTRers = 10;   /* Maximum number interrupt errors to tolerate */
  int numEINTRers;              /* Counter for number of EINTR errors. */
  int numTries;                 /* Counter for number of write */
  const int maxNumTries = 10;   /* maximum number of write attempts to finish. */
  int numToWrite;               /* Number of chars left to write write. */
  char buf[255];                /* Buffer to hold data for I/O operations. */
  char *writeOffset;

  /* First we open the file. */
  numEINTRers = 0;
  do {
    if((FD = open(fileName, O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU | S_IRWXG | S_IRWXO)) < 0) {
      perror("ERROR: File open");
      if(errno != EINTR)
        exit(10);
      numEINTRers++;
    } /* end if */
  } while((FD < 0) && (numEINTRers < maxEINTRers));

  if(FD < 0) {
    errno = EINTR;
    perror("ERROR: Repeated errors");
    exit(11);
  } /* end if */

  /* Next we write something to it.  Most UNIX systems will always write the entire request to disk or they will error, but this is
     not required behavior so we check for partial writes.  In general this kind of code is only seen in network I/O programs
     because it is common to have partial writes in such code. */
  numEINTRers = 0;
  numTries = 0;
  numToWrite = 6;
  strcpy(buf, "Hello\n");
  writeOffset = buf;
  do {
    if((returnValue = write(FD, writeOffset, numToWrite - 2)) < 0) {
      perror("ERROR: Write error");
      if(errno != EINTR)
        exit(10);
      numEINTRers++;
    } else {
      printf("Wrote %d of %d bytes", returnValue, numToWrite);
      numToWrite = numToWrite - returnValue;
      printf(", have %d bytes to go.\n", numToWrite);
      if(numToWrite > 0)
        writeOffset += returnValue;
      numTries++;
      numEINTRers = 0;          /* Reset so we only exit on maxEINTRers REPEATED errors. */
    } /* end else */
  } while((numToWrite > 0) && (numTries < maxNumTries) && (numEINTRers < maxEINTRers));

  /* Figure out if it worked, and if not print out why. */
  if(numToWrite > 0) {
    printf("Write failure.  Have %d bytes left to write.\n", numToWrite);
    if(numTries >= maxNumTries)
      printf("Exit because we had too many successful, but incomplete, write attempts.\n");
    if(numEINTRers >= maxEINTRers)
      printf("Exit because we had too many repeated EINTR errors.\n");
    exit(12);
  } /* end if */

  /* We must close the file to free up system resources.  We must check for errors on the close as well as the other I/O functions.
     If the close fails we may not get the last few writes committed to disk.  If we forget to close the file, then it will be
     closed for us when our program exits.  One should not let the OS close files because if a problem occurs, the program will
     never know and can thus never take action. */
  numEINTRers = 0;
  do {
    if((returnValue = close(FD)) < 0) {
      perror("ERROR: File close");
      if(errno != EINTR)
        exit(13);
      numEINTRers++;
    } /* end if */
  } while((returnValue < 0) && (numEINTRers < maxEINTRers));

  if(returnValue < 0) {
    errno = EINTR;
    perror("ERROR: Repeated errors");
  } /* end if */

  exit(0);
} /* end func main */
