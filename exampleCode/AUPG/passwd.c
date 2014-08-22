/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      passwd.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     UNIX password queries@EOL
   @Keywords  UNIX password user uid gid 
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to query most UNIX versions for password information.  This has been tested
   on Solaris, Linux, FreeBSD, and MacOS X.  This will not extract shadow information found in many UNIX versions including Solaris.
***********************************************************************************************************************************/

#include <pwd.h>                /* UNIX passwd     POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct passwd *pwEnt;
  int i = 0;

  while(NULL != (pwEnt = getpwent())) {
    i++;
    printf("User #%05d: %s:%s:%ld:%ld:%s:%s:%s\n", i, pwEnt->pw_name,
           pwEnt->pw_passwd, (long)pwEnt->pw_uid, (long)pwEnt->pw_gid, pwEnt->pw_gecos, pwEnt->pw_dir, pwEnt->pw_shell);
  } /* end while */

  return 0;
} /* end func main */
