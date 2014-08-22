/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      shmMake.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to create/use a shared memory segment@EOL
   @Keywords  UNIX shared memory
   @Std       ISOC
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how one can create/access shared memory segments in UNIX.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/ipc.h>            /* Need for IPC    POSIX */
#include <sys/shm.h>            /* Shared Mem      POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int shmId;
  void *shmAdd;
  key_t shmKey;

  /* ftok is a handy way to generate a shared memory segment key (or a semaphore key).  On most platforms it works by using the
     inode of the path given as the first arg -- so it will only be unique if the path used by all applications is on the same
     volume (as inodes can be duplicated across different volumes).  For this reason it has become the de facto standard practice to
     use /tmp (or a file in /tmp) to generate keys...  Note: always use a single byte char for the second argument. */
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

  /* We get a segment ID for our fixed key.  IPC_CREAT tells shmget to create a segment, and IPC_EXCL makes shmget fail if the key
     is already in use.  The 0777 sets the perms of the segment (just like a disk file). */
  shmId = shmget(shmKey, 1024, IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
  if(shmId < 0) {
    /* I have a case for all of the interesting cases, even the ones that can not happen in this situation... */
    switch(errno) {
      case EACCES: printf("shmget failed: key already in use permission denied\n");
        break;
      case EEXIST: printf("shmget failed: IPC_CREAT and IPC_EXCL and key is already in use.\n");
        break;
      case ENOSPC: printf("shmget failed: too many segments already!  Could not make another.\n");
        break;
      case ENOENT: printf("shmget failed: IPC_CREAT was not set and segment doesn't already exist.\n");
        break;
      case ENOMEM: printf("shmget failed: Not enough memory to create the segment!\n");
        break;
      default:     printf("shmget failed: Duno why! (errno: %d)\n", errno);
        break;
    } /* end switch */
    exit(1);
  } /* end if */
  printf("shmget result: %d\n", shmId);

  /* We have the shared segment ID, now we map it into our address space. */
  shmAdd = shmat(shmId, NULL, 0);
  if(((long)shmAdd) == -1) {
    switch(errno) {
      case EACCES: printf("shmat failed: permission denied\n");
        break;
      case ENOMEM: printf("shmat failed: Not enough data space to map the segment\n");
        break;
      case EINVAL: printf("shmat failed: Given shared memory identifier was not valid\n");
        break;
      case EMFILE: printf("shmat failed: Can't map because we have mapped too many already\n");
        break;
      default:     printf("shmat failed: Duno why! (errno: %d)\n", errno);
        break;
    } /* end switch */
    exit(1);
  } else {
    printf("Attached to shared mem segment: %ld\n", (long)shmAdd);
  } /* end if/else */

  /* We copy some stuff into the segment so that we can read it out later... */
  strcpy(shmAdd, "Hello");

  /* One should detach from segments when done with them. */
  if(shmdt(shmAdd)) {
    switch(errno) {
      case EACCES: printf("shmdt failed: given address is not a shared memory segment\n");
        break;
      default:     printf("shmdt failed: Duno why! (errno: %d)\n", errno);
        break;
    } /* end switch */
  } else {
    printf("Detached from shared mem segment.\n");
  } /* end if/else */

  return 0;
} /* end func main() */
