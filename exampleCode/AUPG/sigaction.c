/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      sigaction.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
   @brief     UNIX sigaction function.@EOL
   @Keywords  UNIX signal handler sleep sigaction POSIX
   @Std       ISOC POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)
   
   The POSIX sigaction(2) function is more complex than required for most uses, but the simple, non-POSIX function signal() is not
   "safe" for SRV based UNIX variants.  In System V a signal handler is unregistered after a signal is received if the signal()
   function was used to register that signal in the first place.  So the first thing most signal handlers do is to re-register
   themselves.  This leaves open a possible race condition because new signals are not blocked. This behavior is not present in BSD
   4.3.  Thus in BSD, signal(2) is a safe signal function.  The POSIX sigaction(2) function provides the safety of the BSD signal(2)
   function to all platforms.  This C program is intended to illustrate how to bring the simplicity of the signal() function call to
   the POSIX signal handling API; thus providing all POSIX compliant platforms with a simple way to catch signals.
   
   This program also illustrates several signal concepts: 1) How to write a signal handler, 2) How to have a signal handler handle
   more than one signal, 3) How to chain signal handlers, 4) How to set a signal handler, 5) how to ignore signals, 6) how to reset
   signal handlers to the default, and 7) how to set signal handlers back to the last handler.
   
   Note the sleep(2) function sleeps until ANY signal is received, not just a SIGALARM.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <signal.h>             /* UNIX signals    POSIX */
#include <string.h>             /* Strings         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
void (*mjrSignal(int sigcatch, void (*func) (int sigraised))) (int);

void firstUserSignalAction(int sigNum);
void secondUserSignalAction(int sigNum);

/**********************************************************************************************************************************/
/* The following declares a pointer to a function taking an int and returning void */
void (*oldUserSignalAction1) (int);
void (*oldUserSignalAction2) (int);

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

  /* mjrSignal() takes a signal number and a pointer to a signal handler.  It registers that function for that signal, and returns a
     pointer to the previous signal handler.  SIG_DFL is a pointer to the default signal handler.  SIG_IGN is a pointer to the
     "ignore" signal handler.  SIG_ERR is a dummy pointer that indicates an error condition.  The function can error for two
     reasons:
        -# errno=EINVAL if the given integer is not a valid signal number or 
        -# errno=EINVAL if one tries to give a new handler for, or tries to ignore, SIGKILL or SIGSTOP. */
  oldUserSignalAction1 = mjrSignal(SIGUSR1, &firstUserSignalAction);

  if(oldUserSignalAction1 == SIG_ERR) {
    printf("ERROR: mjrSignal() call failed.\n");
    return 1;
  } /* end if */

  /* This is a typical call sequence you might see in real code.  Note that it is common, but not the best.  It doesn't check for
     previous signal handlers. */
  if(SIG_ERR == mjrSignal(SIGUSR2, &firstUserSignalAction)) {
    printf("ERROR: mjrSignal() call failed.\n");
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
  oldUserSignalAction2 = mjrSignal(SIGUSR1, &secondUserSignalAction);

  printf("SIGUSR1=Second,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1=Second,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1=Second,SIGUSR2=First.  Hit me.\n");
  sleep(600);

  /* One common thing to do with the return of mjrSignal() is to set the signal handler back to what it was before you mucked with
     it.  This is the method used inside the sleep() system call.  Here we set SIGUSR1 back to what it was before we set it the
     second time. */
  mjrSignal(SIGUSR1, oldUserSignalAction2);

  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);
  printf("SIGUSR1,SIGUSR2=First.  Hit me.\n");
  sleep(600);

  /* We now set SIGUSR1 to the default and SIGUSR2 to ignore. */
  mjrSignal(SIGUSR1, SIG_DFL);
  mjrSignal(SIGUSR2, SIG_IGN);

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

/**********************************************************************************************************************************/
/* This is a simple wrapper function for sigaction(2) that returns the simple syntax of signal(2) and the robustness of POSIX/BSD4.3
   signals.  This is a nice way to get BSD4.3 signal handling inside of SRV as well.  The signal(2) function call has slightly
   different behavior between modern BSD based systems and modern SRV based systems.  This function, because it uses the POSIX
   sigaction API, gives both systems the same behavior--that of BSD4.3 and POSIX.  This function will return SIG_ERR on error and
   set errno.  Upon success it will return the previous action.  EFAULT is one possible value for errno that this funciton may set,
   but that the origonal signal(2) function will not.  See the errno values possible for the sigaction(2) function for more
   information.  */
void (*mjrSignal(int sigcatch, void (*func) (int sigraised))) (int) {
  struct sigaction newSigAction;
  struct sigaction oldSigAction;

/* For platforms with a sig mask that is not a simple integer, we must take care and fill the field with zeros using memset instead
   of simply setting the quantity to the zero integer. For a BSD system, 'newSigAction.sa_mask = 0;', would be enough to get the job
   done. */
  memset(&newSigAction.sa_mask, 0, sizeof(newSigAction.sa_mask)); 

  newSigAction.sa_flags = 0;
  newSigAction.sa_handler = func;
  if(sigaction(sigcatch, &newSigAction, &oldSigAction) < 0) {
    return SIG_ERR;
  } else {
    return oldSigAction.sa_handler;
  } /* end if/else */
} /* end func mjrSignal */
