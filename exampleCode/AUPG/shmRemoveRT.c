/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      shmRemoveRT.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998,2000,2004 by Mitch Richling.  All rights reserved.
   @brief     How to delete a shared memory segment@EOL
   @Keywords  UNIX shared memory
   @Std       ISOC
   @Tested    
              - MacOS X.2

   This C program is intended to illustrate how one can delete shared memory segments in UNIX using the POSIX real-time APIs.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <errno.h>              /* error stf       POSIX */
#include <sys/mman.h>           /* mmap()          POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

  /* Getting rid of a shared memory segment via the POSIX RT APIs is simple: */
  if(shm_unlink("/tmp/foobar") < 0) {
    /* I have a case for all of the interesting cases, even the ones that can not happen in this situation... */
    switch(errno) {
     case EACCES:           printf("shm_unlink failed: Permission is denied to be remove the object.\n");
        break;
     case ENAMETOOLONG:     printf("shm_unlink failed: name exceeded SHM_NAME_MAX characters.\n");
        break;
     case ENOENT:           printf("shm_unlink failed: The named object does not exist.\n");
        break;
    default:                printf("shm_unlink failed: Duno why (%d)...\n", errno);
        break;
    } /* end switch */
    exit(1);
  } /* end if */

  printf("The shared memory segment was removed.\n");

  return 0;
} /* end func main() */
