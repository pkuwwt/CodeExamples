/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      termSizeWatch.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to monitor terminal size@EOL
   @Keywords  termios terminal tty window
   @Std       C89

   Demonstrates how to monitor the changing size of a terminal window via the SIGWINCH signal mechanism.  In addition, various other
   terminal operations are demonstrated: how to determine if a process is connected to a terminal, getting the current terminal's
   name, and getting the size of the current terminal.

   A note for budding curses and ncurses programmers.  The "standard" way to make curses work when the terminal may be resized, is
   to install a signal handler, like the one demonstrated below, that will update things if the window is resized (an endwin &
   refresh). Alternately, ncurses provides a hook called resizeterm to help automate this process..
***********************************************************************************************************************************/

#include <termios.h>            /* terminal I/O    POSIX */
#include <sys/ioctl.h>          /* for ioctl()           */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <signal.h>             /* UNIX signals    POSIX */
#include <string.h>             /* Strings         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
static void winWatch(int sigNo);

/**********************************************************************************************************************************/
int main() {
  struct winsize termSize;
  char *termNameTTY;
  int termIsTTY;
  struct sigaction newSigAction;
  struct sigaction oldSigAction;

  /* Report the current STDIN file descriptor. */
  printf("Current STDIN FD: %d\n", (int)STDIN_FILENO);

  /* Figure out if we are attached to a TTY. */
  termIsTTY = isatty(STDIN_FILENO);
  if(termIsTTY) {
    printf("We are on a TTY!\n");
  } else {
    printf("We are NOT on a TTY!\n");
    printf("ERROR: We must be connected to a TTY.\n");
    exit(1);
  } /* end if */

  /* Get our TTY name -- just because we can. */
  termNameTTY = ttyname(STDIN_FILENO);
  if(termNameTTY == NULL) {
    printf("ERROR: Could not get the name of our TTY.\n");
    exit(1);
  } /* end if */
  printf("Current TTY name: %s\n", termNameTTY);

  /* Get an initial window size. */
  if(ioctl(STDIN_FILENO, TIOCGWINSZ, (char *) &termSize) < 0) {
    printf("ERROR: We ioctl failure for our TTY.\n");
    exit(1);
  } /* end if */
  printf("Term Size: (%d,%d)\n", (int)termSize.ws_row, (int)termSize.ws_col);

  /* Set a signal handler to catch SIGWINCH */
  memset(&newSigAction.sa_mask, 0, sizeof(newSigAction.sa_mask)); 
  newSigAction.sa_flags = 0;
  newSigAction.sa_handler = winWatch;
  if(sigaction(SIGWINCH, &newSigAction, &oldSigAction) < 0) {
    printf("ERROR: Unable to install signal handler\n");
    exit(1);
  } /* end if */

  /* Sleep forever, catching signals as required. */
  while(1) {
    printf("Sleeping...\n");
    sleep(10);
  } /* end while */

} /* end func main */

/**********************************************************************************************************************************/
/* Signal handler function intended to catch a SIGWINCH signal.  It just prints the window size. See the main function for more
   information regarding how to use isatty() and ioctl(). */
static void winWatch(int sigNo) {
  struct winsize termSize;
  if(sigNo == SIGWINCH) {
    if(isatty(STDIN_FILENO)) {
      if(ioctl(STDIN_FILENO, TIOCGWINSZ, (char *) &termSize) >= 0) {
        printf("NEW Term Size: (%d,%d)\n", (int)termSize.ws_row, (int)termSize.ws_col);   
      } else {
        printf("ERROR: We ioctl failure for our TTY.\n");
      } /* end if */
    } else {
      printf("ERROR: We isatty failure for our TTY.\n");
    } /* end if */
  } else {
    printf("ERROR: We got the wrong signal directed to winWatch (%d).\n", (int)sigNo);
  } /* end if */
} /* end func winWatch */
