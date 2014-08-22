/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      group.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX group queries@EOL
   @Keywords  UNIX group user gid
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to query most UNIX versions for user group data.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <grp.h>                /* UNIX groups     POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct group *grEnt;
  int j, i = 0;

  while(NULL != (grEnt = getgrent())) {
    i++;
    printf("Group #%05d: %s:%s:%ld:", i, grEnt->gr_name, grEnt->gr_passwd, (long)grEnt->gr_gid);
    /* Now we go through the member list.  This is an array of pointers to chars(strings).  The end is marked with a pointer to
       NULL. */
    for(j = 0; grEnt->gr_mem[j] != NULL; j++)
      printf("%s%s", grEnt->gr_mem[j], (grEnt->gr_mem[j + 1] != NULL ? "," : ""));
    printf("\n");
  } /* end while */

  return 0;
} /* end func main */
