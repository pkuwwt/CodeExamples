/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      rssu.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1993 by Mitch Richling.  All rights reserved.
   @brief     Sudo replacement@EOL
   @Keywords  UNIX sudo suid
   @Std       ISOC UNIX98 SYSV BSD4.3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)
              - Ubuntu 10.10
   
   This handy little program is an abomination of all that is holy in UNIX security.  It checks the UID of the user and if it is OK,
   runs a root shell for the user.  This requires no password.  This program is generally a very bad idea.
***********************************************************************************************************************************/

#include <unistd.h>             /* UNIX std stf    POSIX */
#include <syslog.h>             /* UNIX syslog     UNIX  */
#include <pwd.h>                /* UNIX passwd     POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
#define VALID_UID          501
#define ROOT_UID           0
#define ROOT_GID           0
#define COMMAND_STR        "/bin/bash"
#define COMMAND_STR_DESC   "ls"

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char strBuf[1024];

  if((geteuid() != ROOT_UID) || (getegid() != ROOT_GID) || (getuid() != VALID_UID)) {
    sprintf(strBuf, "rssu: Failure by uid=%ld", (long)getuid());
    syslog(LOG_NOTICE | LOG_AUTH, "%s", strBuf);
    return 1;
  } else {
    sprintf(strBuf, "rssu: Success by uid=%ld", (long)getuid());
    syslog(LOG_NOTICE | LOG_AUTH, "%s", strBuf);
    setuid(ROOT_UID);
    setgid(ROOT_GID);
    setreuid(ROOT_UID, ROOT_UID);
    execlp(COMMAND_STR, COMMAND_STR_DESC, (char *)0);
    return 0;
  } /* end if/else */
} /* end func main */
