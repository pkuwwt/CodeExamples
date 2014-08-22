/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      shmUse.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to use a shared memory segment@EOL
   @Keywords  UNIX shared memory
   @Std       ISOC
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how one can attach to an already existing shared memory segment, read it's contents, and
   write new data into it.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/ipc.h>            /* Need for IPC    POSIX */
#include <sys/shm.h>            /* Shared Mem      POSIX */
#include <ctype.h>              /* Char classes    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int shmId;
  void *shmAdd;
  int i;
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

  /* We get a segment ID for our fixed key (see ftok for a SysV'ism for generating shared memory segment keys. */
  shmId = shmget(shmKey, 1024, 0);
  if(shmId < 0) {
    /* See shmMake.c for a more complete set of errno values to check.  We only check what is important in this case. */
    switch(errno) {
      case EACCES: printf("shmget failed: key already in use permission denied\n");
        break;
      case ENOENT: printf("shmget failed: IPC_CREAT was not set and segment doesn't already exist.\n");
        break;
      default:     printf("shmget failed: Duno why!\n");
        break;
    } /* end switch */
    exit(1);
  } /* end if */
  printf("shmget result: %d\n", shmId);

  /* We have the shared segment ID, now we map it into our address space. */
  shmAdd = shmat(shmId, NULL, 0);
  if(((long)shmAdd) == -1) {
    printf("ERROR: Could not attach to shared memory segment. Error code: %d\n", errno);
    exit(1);
  } else {
    printf("Attached to shared mem segment: %ld\n", (long)shmAdd);
  } /* end if */

  /* Just print the printable chars in case the segment has binary junk in it... */
  printf("The segment content: \n");
  for(i=0;(i<1024)&&(((char *)shmAdd)[i]!=0);i++)
    if(isalpha(((char *)shmAdd)[i]) || isdigit(((char *)shmAdd)[i]))
      printf("%c", ((char *)shmAdd)[i]);
  printf("\n");

  /* Now we put other stuff into the segment. */
  strcpy(shmAdd, "World");

  /* One should detach from segments when done with them. */
  if(shmdt(shmAdd)) {
    printf("ERROR: shmdt failure.  Error code: %d\n", errno);
  } else {
    printf("Detached from shared mem segment.\n");
  } /* end if */

  return 0;
} /* end func main() */
