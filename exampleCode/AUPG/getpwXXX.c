/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      getpwXXX.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX password lookup @EOL
   @Keywords  UNIX password shadow passwd getpwnam getpwuid
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to query most UNIX versions for password information by user name (uname)
   or user ID (UID).
***********************************************************************************************************************************/

#include <pwd.h>                /* UNIX passwd     POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <ctype.h>              /* Char classes    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct passwd *pwEnt;
  char *c;
  uid_t theUID;
  int ArgIsNotNumber;

  if(argc != 2) {
    printf("ERROR: This program requires a user name or UID as the only argument.\n");
    exit(1);
  } /* end if */

  /* Figure out if we got a user name or a UID */
  for(c=argv[1],ArgIsNotNumber=0;!((*c=='\0')||ArgIsNotNumber);c++)
    if(!isdigit(*c))
      ArgIsNotNumber=1;

  /* Do the lookup */
  if(ArgIsNotNumber) {
    printf("Looking up user with user name: %s...\n\n", argv[1]);
    if( (pwEnt = getpwnam(argv[1])) == NULL) {
      printf("ERROR: Call to getpwnam failed.\n");
      exit(1);
    } /* end if */
  } else {
    theUID = atol(argv[1]);
    printf("Looking up user with UID: %ld...\n\n", (long)theUID);
    if((pwEnt = getpwuid(theUID)) == NULL) {
      printf("ERROR: Call to getpwuid failed.\n");
      exit(2);
    } /* end if */
  } /* end if/else */

  printf("pw_name:   %s\n",  pwEnt->pw_name);
  printf("pw_passwd: %s\n",  pwEnt->pw_passwd);
  printf("pw_uid:    %ld\n", (long)pwEnt->pw_uid);
  printf("pw_gid:    %ld\n", (long)pwEnt->pw_gid);
  printf("pw_gecos:  %s\n",  pwEnt->pw_gecos);
  printf("pw_dir:    %s\n",  pwEnt->pw_dir);
  printf("pw_shell:  %s\n",  pwEnt->pw_shell);

  return 0;
} /* end func main */
