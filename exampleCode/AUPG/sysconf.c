/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      sysconf.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     UNIX sysconf function@EOL
   @Keywords  UNIX sysconf POSIX
   @Std       IEEE Std 1003.1-1988 (POSIX.1) ISOC
   @Tested    
              - Solaris 2.8
              - MacOS X.3
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to use sysconf to get various system parameters.  POSIX defines many
   constants for use as the argument to sysconf; however, many systems don't implement them all.  The constants demonstrated in this
   program represent a common core of useful constants that are defined by most UNIX variants.
***********************************************************************************************************************************/

#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

  printf("_SC_ARG_MAX .....  The maximum bytes of argument to execve ................... %d\n", (int)sysconf(_SC_ARG_MAX));
  printf("_SC_CHILD_MAX ...  The maximum number of simultaneous processes per user id .. %d\n", (int)sysconf(_SC_CHILD_MAX));
  printf("_SC_OPEN_MAX ..... The maximum number of open files per user id .............. %d\n", (int)sysconf(_SC_OPEN_MAX));
  printf("_SC_NGROUPS_MAX... The maximum number of supplemental groups ................. %d\n", (int)sysconf(_SC_NGROUPS_MAX));
  printf("_SC_VERSION....... The version of POSIX.1 with for system .................... %d\n", (int)sysconf(_SC_VERSION));
#ifndef __CYGWIN__
  printf("_SC_2_VERSION .... The version of POSIX.2 with for system .................... %d\n", (int)sysconf(_SC_2_VERSION));
  printf("_SC_STREAM_MAX ... The minimum maximum number of streams per process ......... %d\n", (int)sysconf(_SC_STREAM_MAX));
#endif
  printf("_SC_CLK_TCK ...... The frequency of the statistics clock in ticks per second . %d\n", (int)sysconf(_SC_CLK_TCK));
  printf("_SC_PAGESIZE ..... The memory page size of the system expressed in bytes ..... %d\n", (int)sysconf(_SC_PAGESIZE));
  printf("_SC_SAVED_IDS .... Saved SGID & SUID available (1 is YES, -1 is NO) .......... %d\n", (int)sysconf(_SC_SAVED_IDS));  

  return 0;
} /* end func main */
