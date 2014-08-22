/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      forknwait.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     UNIX group queries@EOL
   @Keywords  fork wait UNIX
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how to fork a process and how to wait for the child.  How to wait for the child is much
   more involved than simply calling the wait system call.  This is because the wait call can return for many reasons.
   
   To make applications more portable it is preferable not to use our special knowledge of the high/low order 8 bit quantities in
   the 16 bit return from wait.  Instead we can use the POSIX macros to do the same thing.  I have sprinkled comments in the code
   discussing the do-it yourself methods and the POSIX methods.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <sys/wait.h>           /* UNIX wait       POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  pid_t parentPID, wPID, childPID;
  int i, whyWait, mustWait, unknownFail;

  /* Get the PID */
  parentPID = getpid();
  printf("PARENT: Process ID: %ld\n", (long)parentPID);

  childPID = fork();
  printf("PARENT: fork returned: %ld\n", (long)childPID);
  if(childPID) {                /* Parent */
    printf("PARENT: I'm still running after the fork.\n");
    if(childPID == -1) {
      printf("ERROR: The fork was not successful.\n");
      exit(2);
    } /* end if */
    printf("PARENT: My child's PID: %ld.\n", (long)childPID);

    /* We wait for the child to quit.  This is not as simple as just calling the wait(2) system call, because the wait call can
       return for many reasons.  */
    unknownFail = 0;
    mustWait = 1;
    do {
      wPID = wait(&whyWait);
      printf("PARENT: wait has returned(%ld).\n", (long)wPID);
      if(wPID == -1) {
        if(errno == ECHILD) {   /* My babies are all dead! */
          mustWait = 0;
        } else if(errno == EINTR) {     /* I was sent a signal. */
          printf("PARENT: received a signal.  Continuing.\n");
        } else {
          printf("PARENT: WARNING: unknown wait failure.\n");
          unknownFail++;
        } /* end if/else */
      } else {
        if(wPID == childPID) {  /* This is my baby. */
          if(WIFSTOPPED(whyWait)) {     /* STOPed my baby. */
            printf("PARENT: child was stopped with signal %d", (int)WSTOPSIG(whyWait));
          } else if(WIFEXITED(whyWait)) {       /* My baby died a natural death. */
            printf("PARENT: child exited with %d.\n", (int)WEXITSTATUS(whyWait));
            mustWait = 0;
          } else if(WIFSIGNALED(whyWait)) {     /* My baby was killed. */
            printf("PARENT: child was killed by signal %d.\n", (int)WTERMSIG(whyWait));
            if(WCOREDUMP(whyWait))
              printf("PARENT: child dumped core\n");
            else
              printf("PARENT: child did not dump core\n");
            mustWait = 0;
          } else {              /* We don't know why wait() returned.  This should not happen. */
            printf("PARENT: WARNING: wait failure(unknown status).\n");
            unknownFail++;
          } /* end if/else */
        } else {                /* wait() reports a pid that is not my child. */
          printf("PARENT: WARNING: wait failure(unknown child).\n");
          unknownFail++;
        } /* end if/else */
      } /* end if/else */
    } /* end do */
    while(mustWait && (unknownFail < 10));
    if(mustWait == 0)
      printf("PARENT: normal exit because child has gone away.\n");
    else
      printf("PARENT: exit because of repeated, unknown failures of wait(2).\n");
  } else {                      /* Child */
    printf("CHILD:  I'm running.\n");
    printf("CHILD:  Process ID: %ld\n", (long)getpid());
    printf("CHILD:  Process Group ID: %ld\n", (long)getpgrp());
    printf("CHILD:  I'm going to sleep for 10 seconds now.\n");
    for(i = 10; i > 0; i--) {
      printf("CHILD: Count down: %d\n", i);
      sleep(1);
    } /* end for */
    printf("CHILD:  I'm done sleeping.\n");
    printf("CHILD:  forknwait shutdown.\n");
  } /* end if/else */

  return (0);
} /* end func main */
