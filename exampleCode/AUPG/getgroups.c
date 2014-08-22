/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      getgroups.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX group queries@EOL
   @Keywords  UNIX group user gid getgroup getgrgid
   @Std       ISOC BSD4.3 POSIX.1
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to use getgroups to query the current user's group set.  The system call
   getgrgid is also demonstrated.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <grp.h>                /* UNIX groups     POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <sys/param.h>          /* UNIX Params     ????  */
#include <errno.h>              /* error stf       POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int numGroups, i;
  long ngroups_max; 
  /* gid_t gidList[NGROUPS_MAX]; */
  gid_t *gidList;
  struct group *grEnt;

  ngroups_max = sysconf(_SC_NGROUPS_MAX);
  gidList = (gid_t *)malloc(sizeof(gid_t)*ngroups_max);

  /* How to figure out how many groups the current user is in. */
  if( (numGroups = getgroups(0, NULL)) < 0) {
    printf("ERROR: getgroups() failure.\n");
    exit(1);
  } /* end if */
  printf("User is in %d groups.\n", numGroups);
  
  /* We could allocate just enough space to hold the group list; however, the number may have changed since we called getgroups() a
     moment ago!  Also the maximum number is generally quite small (less than 32).  Thus we have no reason not to just use the
     maximal array required. */
  if((numGroups = getgroups(ngroups_max, gidList)) < 0) {
    printf("ERROR: getgroups() failure.\n");
    if(errno == EINVAL) {
      /* In the case of EINVAL we know what happened. Many UNIXes provide additional diagnostics -- see your man pages. */
      printf("ERROR: ngroups_max != 0 && ngroups_max < # supplementary of groups\n");
    } /* end if */
    exit(2);
  } /* end if */

  if(numGroups >= 0) {
    /* Print the GIDs out */
    printf("The group ID list: ");
    for(i=0;i<numGroups;i++) {
      printf("%i ", gidList[i]);
    } /* end for */
    printf("\n");
    /* Convert the group IDs into group names. */
    printf("The group name list: ");
    for(i=0;i<numGroups;i++) {
      if((grEnt = getgrgid(gidList[i])) == NULL) {
        printf("??? ");
      } else {
        printf("%s ", grEnt->gr_name);
      } /* end if/else */
    } /* end for */
    printf("\n");
  } /* end if */

  return 0;
} /* end func main */
