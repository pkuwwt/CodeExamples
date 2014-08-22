/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      getgrXXX.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX group queries with getgrgid and getgrnam@EOL
   @Keywords  UNIX group user gid getgrgid getgrnam
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to use the getgrgid and getgrnam functions.  getgrnam queries user group
   data by group GID.  A GID is a unique integer that UNIX uses to identify groups. It is actually this number that is used by most
   system routines.  getgrnam queries user group data by group name.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <grp.h>                /* UNIX groups     POSIX */
#include <signal.h>             /* UNIX signals    POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <ctype.h>              /* Char classes    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct group *grEnt;
  char **cp, *c;
  gid_t theGID;
  int ArgIsNotNumber;

  if(argc != 2) {
    printf("ERROR: This program requires a group GID or name as the only argument.\n");
    exit(1);
  } /* end if */

  /* Figure out if we got a group name or a GID */
  for(c=argv[1],ArgIsNotNumber=0;!((*c=='\0')||ArgIsNotNumber);c++)
    if(!isdigit(*c))
      ArgIsNotNumber=1;

  /* Do the lookup */
  if(ArgIsNotNumber) {
    printf("Looking up group with name: %s...\n\n", argv[1]);
    if((grEnt = getgrnam(argv[1])) == NULL) {
      printf("ERROR: Call to getgrnam failed\n");
      exit(2);
    } /* end if */
  } else {
    theGID = atol(argv[1]);
    printf("Looking up group with GID: %ld...\n\n", (long)theGID);
    if((grEnt = getgrgid(theGID)) == NULL) {
      printf("ERROR: Call to getgrgid failed.\n");
      exit(2);
    } /* end if */
  } /* end if/else */

  /* Print the results. */
  printf("Group name:   %s\n",  grEnt->gr_name);
  printf("Group passwd: %s\n",  grEnt->gr_passwd);
  printf("Group GID:    %ld\n", (long)grEnt->gr_gid);

  /* gr_mem is an array of pointers to chars(strings) terminated by a NULL pointer. */
  printf("Members:\n");
  for(cp=grEnt->gr_mem; *cp != NULL; cp++)
    printf("   %s\n", *cp);
  printf("\n");

  return 0;
} /* end func main */
