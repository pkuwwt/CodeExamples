/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      kill.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX signals and signal handling@EOL
   @Keywords  UNIX signal handle
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illistrate how to send signals to other processes.  This is a very simple version of the kill(1)
   UNIX command.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <errno.h>              /* error stf       POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <signal.h>             /* UNIX signals    POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  pid_t thePid;
  int sig;
  char *p;

  if(argc != 3) {
    printf("ERROR: Incorrect number of arguments.\n");
    printf("Use: kill -n m\n");
    printf("     n is a number indicateing the signal\n");
    printf("     m is the PID of the process to send the signal to.\n");
    exit(1);
  } /* end if */

  /* Check to see if they put a '-' in front of the signal number, and if they did, we extract the signal number. */
  if(argv[1][0] == '-') {
    p = &(argv[1][1]);
    sig = atol(p);
  } else {
    printf("ERROR: Bad first argument.\n");
    exit(1);
  } /* end if/else */

  /* Extract the PID of the process to send the signal to. */
  thePid = atol(argv[2]);

  /* Send the signal. */
  if(kill(thePid, sig) < 0) {
    perror("ERROR: kill");
    exit(1);
  } /* end if */

  exit(0);

} /* end func main */
