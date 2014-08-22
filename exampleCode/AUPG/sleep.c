/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      sleep.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     How to use UNIX sleep()@EOL
   @Keywords  UNIX signal sleep
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate the correct way to make a program sleep for at least n seconds.  This is more complex
   that simply calling the sleep() system call.  The sleep system call 1) stores the current signal handler for alarm, 2) sets an
   alarm for n seconds, 3) waits until a signal is received, 4) sets the signal handler back to the correct thing.  Thus the sleep
   system call can be prematurely interrupted by a signal.  To get it to wait the correct amount of time, we need to add some code.
   This program illustrates how to do do this.  This program sets up a signal handler for SIGUSR1 to help you understand the
   process.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <time.h>               /* time            C89   */
#include <signal.h>             /* UNIX signals    POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int secToSleep = 600;
  long startTime;
  long endTime;

  /* First set the SIGUSR1 signal to be ignored so that the user can send this program SIGUSR1 signals to test it. */
  if(SIG_ERR == signal(SIGUSR1, SIG_IGN)) {
    printf("ERROR: Couldn't set signal handler for SIGUSR1\n");
    return 1;
  } /* end if */
  printf("My PID(try SIGUSR1): %ld\n", (long)getpid());

  startTime = time(NULL);
  endTime = startTime + secToSleep;

  /* Loop until we have waited secToSleep seconds. */
  printf("Current Time=%ld\n", (long)time(NULL));
  while(time(NULL) <= endTime) {
    sleep(endTime - time(NULL));
    printf("Current Time=%ld\n", (long)time(NULL));
  } /* end while */
  printf("Time gone by: %ld\n", time(NULL) - startTime);
  
  return 0;

} /* end func main */
