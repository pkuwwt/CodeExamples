/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      shmRemove.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to remove a shared memory segment@EOL
   @Keywords  UNIX shared memory remove delete
   @Std       C89 POSIX
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how one can delete a shared memory segment.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <errno.h>              /* error stf       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/ipc.h>            /* Need for IPC    POSIX */
#include <sys/shm.h>            /* Shared Mem      POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int shmId, retV;
  key_t shmKey;

  /* ftok generates a shared memory segment key.  For more info about ftok(), see the shmMake.c example program. */
  shmKey = ftok("/tmp", 'm');
  if (shmKey < 0) {
    switch(errno) {
      case EACCES:       printf("ftok failed: Search permission denied for a component of the path prefix.\n");
        break;
      case ELOOP:        printf("ftok failed: A loop exists in symbolic links in path argument or too many symlinks found.\n");
        break;
      case ENAMETOOLONG: printf("ftok failed: The length of path exceeds {PATH_MAX} or pathname component longer than {NAME_MAX}.\n");
        break;
      case ENOENT:       printf("ftok failed: A component of path does not name an existing file or path is an empty string.\n");
        break;
      case ENOTDIR:      printf("ftok failed: A component of the path not a directory.\n");
        break;
      default:           printf("ftok failed: Duno why! (errno: %d)\n", errno);
        break;
    } /* end switch */
    exit(1);
  } /* end if */

  /* We get a segment ID for our fixed key.  Note that the last argument is zero so that we don't create a segment -- just connect
     to one if it exists. */
  shmId = shmget(shmKey, 1024, 0);
  if(shmId < 0) {
    /* See shmMake.c for a more complete set of errno values to check.  We only check what is important in this case. */
    switch(errno) {
      case EACCES: printf("shmget failed: key already in use permission denied\n");
        break;
      case ENOENT: printf("shmget failed: IPC_CREAT was not set and segment doesn't already exist.\n");
        break;
      default:     printf("shmget failed: Duno why! (errno: %d)\n", errno);
        break;
    } /* end switch */
    exit(1);
  } /* end if */
  printf("shmget result: %d\n", shmId);

  /* This is how you get rid of a shared memory segment... */
  retV = shmctl(shmId, IPC_RMID, NULL);
  if(retV == 0) {
    printf("shmctl worked.  Segment removed.\n");
  } else {
    switch(errno) {
      case EPERM:  printf("permission denied with IPC_SET or IPC_RMID.\n");
        break;
      case EACCES: printf(" Read permission denied with IPC_STAT.\n");
        break;
      case EINVAL: printf(" The shared memory ID is not valid.\n");
        break;
      case EFAULT: printf(" Invalid address for final argument.\n");
        break;
      default:     printf("shmctl failed: Duno why! (errno: %d)\n", errno);
        break;
    } /* end switch */
  } /* end if */

  return 0;
} /* end func main() */
