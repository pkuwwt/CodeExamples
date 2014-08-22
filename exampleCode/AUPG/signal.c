/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      signal.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
   @brief     UNIX signals and sleep@EOL
   @Keywords  UNIX signal handler sleep
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)
   
   This C program is intended to illustrate two basic things: 1) the sleep() system call sleeps until the application receives a
   signal and 2) some basic signal concepts.  Demonstrated concepts: 1) How to write a signal handler, 2) How to have a signal
   handler handle more than one signal, 3) How to chain signal handlers, 4) How to set a signal handler, 5) how to ignore signals,
   6) how to reset signal handlers to the default, and 7) how to set signal handlers back to the last handler.
   
   The API used is NOT the POSIX one, but rather the older signal(2) function is the primary work horse here.  Some SRV programmers
   may recognize the signal(2) function used in this code as part of the so called "unsafe" signal API.  In System V a signal
   handler is unregistered after a signal is received if this API is used.  So the first thing most signal handlers do is to
   re-register themselves.  This leaves open a possible race condition as new signals are not blocked.  This behavior is not present
   in BSD 4.3.  Thus in BSD, this is a safe signal API.
   
   To see an example of the POSIX functions, take a look at the example program sigaction.c.
***********************************************************************************************************************************/

#include <signal.h>             /* UNIX signals    POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
void firstUserSignalAction(int sigNum);
void secondUserSignalAction(int sigNum);

/**********************************************************************************************************************************/
/* The following declares a pointer to a function taking an int and returning void */
void (*oldUserSignalAction1) (int);
void (*oldUserSignalAction2) (int);

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

  /* signal() takes a signal number and a pointer to a signal handler.  It registers that function for that signal, and returns a
     pointer to the previous signal handler.  SIG_DFL is a pointer to the default signal handler.  SIG_IGN is a pointer to the
     "ignore" signal handler.  SIG_ERR is a dummy pointer that indicates an error condition.  The function can error for two
     reasons: 1) errno=EINVAL if the given integer is not a valid signal number or 2) errno=EINVAL if one tries to give a new
     handler for, or tries to ignore, SIGKILL or SIGSTOP. */

  oldUserSignalAction1 = signal(SIGUSR1, &firstUserSignalAction);

  if(oldUserSignalAction1 == SIG_ERR) {
    printf("ERROR: signal() call failed.\n");
    return 1;
  } /* end if */

  /* This is a typical call sequence you might see in real code.  Note that it is common, but not the best.  It doesn't check for
     previous signal handlers. */
  if(SIG_ERR == signal(SIGUSR2, &firstUserSignalAction)) {
    printf("ERROR: signal() call failed.\n");
    return 1;
  } /* end if */

  printf("My PID: %ld\n", (long)getpid());

  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);

  /* Note, with the oldUserSignalAction1 value, we can check to see what was the handler before.  This probably should be done in
     good code because child processes inherit signal handlers.  */
  if(oldUserSignalAction1 == SIG_DFL)
    printf("Old signal action was the default.\n");
  else if(oldUserSignalAction1 == SIG_IGN)
    printf("Old signal action was to ignore.\n");
  else
    printf("Old signal action was a custom signal handler.\n");

  /* One common thing to do with old signal handlers is to "chain" them.  For example we redefine the SIGUSR1 handler, but the first
     thing that the handler does is to call the old handler.  oldUserSignalAction2 is global so that firstUserSignalAction can be
     used for chaining.  Note that one should NOT chain outside of the handler definition because */
  oldUserSignalAction2 = signal(SIGUSR1, &secondUserSignalAction);

  printf("SIGUSR1=Second,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1=Second,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1=Second,SIGUSR2=First.  Hit me.\n");
  sleep(600);

  /* One common thing to do with the return of signal() is to set the signal handler back to what it was before you mucked with it.
     This is the method used inside the sleep() system call.  Here we set SIGUSR1 back to what it was before we set it the second
     time. */
  signal(SIGUSR1, oldUserSignalAction2);

  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);

  /* We now set SIGUSR1 to the default and SIGUSR2 to ignore. */
  signal(SIGUSR1, SIG_DFL);
  signal(SIGUSR2, SIG_IGN);

  printf("SIGUSR1=deflt, SIGUSR2=ignore.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1=deflt, SIGUSR2=ignore.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1=deflt, SIGUSR2=ignore.  Hit me.\n");
  sleep(600);

  return 0;
} /* end func main */

/**********************************************************************************************************************************/
/* All signal handlers take a single integer argument and return void.  The integer argument has the signal number.  This lets one
   handler handle multiple signals as demonstrated here. */
void firstUserSignalAction(int sigNum) {
  /* We make SIGUSR1 & SIGUSR2 both handled by this function.  We can tell what signal we got from the argument. */
  if(sigNum == SIGUSR1)
    printf("First handler. This is sigusr1(%d)\n", sigNum);
  else
    printf("First handler. This is sigusr2(%d)\n", sigNum);
} /* end func firstUserSignalAction */

/**********************************************************************************************************************************/
void secondUserSignalAction(int sigNum) {
  (*oldUserSignalAction2) (sigNum);
  printf("Second handler.  We got signal: %d\n", sigNum);
} /* end func secondUserSignalAction */
