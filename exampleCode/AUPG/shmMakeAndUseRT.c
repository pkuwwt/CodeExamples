/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      shmMakeAndUseRT.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998,2000,2004 by Mitch Richling.  All rights reserved.
   @brief     How to create/use a shared memory segment@EOL
   @Keywords  UNIX shared memory
   @Std       ISOC
   @Tested    
              - MacOS X.2

   This C program is intended to illustrate how one can create/access shared memory segments in UNIX using the POSIX real-time APIs.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <errno.h>              /* error stf       POSIX */
#include <fcntl.h>              /* UNIX file ctrl  UNIX  */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/mman.h>           /* mmap()          POSIX */
#include <ctype.h>              /* Char classes    C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int shmFD;
  void *shmAdd;
  int i, useMode=0;   /* 1=use segment, not create it. */

  /* We get "open" a shared memory segment with the given name.  The flags work just as they do with open: O_CREAT creates the
     segment if it doesn't exist, O_EXCL errors out if the segment already exists, and O_RDRW opens it for read and write.  The
     perms are as with chmod.  Normally, things opened with shm_open do now appear in the filesystem; however, it is best to specify
     a path to a file you can write to! */
  shmFD = shm_open("/tmp/foobar", O_RDWR | O_CREAT | O_EXCL, S_IRWXU | S_IRWXG | S_IRWXO);
  if(shmFD < 0) {
    /* I have a case for all of the interesting cases, even the ones that can not happen in this situation... */
    switch(errno) {
      case EACCES:       printf("shm_open failed: The requested permissions were denied or create denied .\n");
        break;
      case EEXIST:       printf("shm_open failed: Object already exists and O_CREAT and O_EXCL were specified.\n");
        useMode=1;
        break;
      case EINTR:        printf("shm_open failed: The operation was interrupted by a signal.\n");
        break;
      case EINVAL:       printf("shm_open failed: The operation is not supported.\n");
        break;
      case EMFILE:       printf("shm_open failed: Too many file descriptors were already open.\n");
        break;
      case ENAMETOOLONG: printf("shm_open failed: Name was too long (longer than SHM_NAME_MAX chars).\n");
        break;
      case ENFILE:       printf("shm_open failed: The system file table is full.\n");
        break;
      case ENOENT:       printf("shm_open failed: Object doesn't exist (O_CREAT was not specified).\n");
        break;
      case ENOSPC:       printf("shm_open failed: No memory to create object (O_CREAT was specified).\n");
        break;
      default:           printf("shm_open failed: Duno why...\n");
        break;
    } /* end switch */
  } /* end if */

  /* If we failed before because the segment already existed, then we try to open it up and use it -- thus combining the "make" and
     "use" example programs. */
  if(shmFD < 0) {
    if(useMode) {
      printf("Segment existed, attempting to open it..\n");
      shmFD = shm_open("/tmp/foobar", O_RDWR, S_IRWXU | S_IRWXG | S_IRWXO);
      if(shmFD < 0) {
        printf("shm_open failed: Open (without O_CREAT failed too!), errno: %d\n", errno);
        exit(1);
      } /* end if */
    } else {
      exit(1);
    } /* end if/else */
  } /* end if */

#define SEGSIZE ((size_t)1024)

  if( ! useMode) {
    /* Use ftruncate() to size the region -- IMO an only semi-intuitive choice on the part of the POSIX committee. */
    if(ftruncate(shmFD, SEGSIZE) < 0) {
      /* I have included all typically supported error cases below, even if they don't apply here. */
      switch(errno) {
        case EBADF:  printf("ftruncate failed: The fd is not a valid descriptor.\n");
          break;
        case EINVAL: printf("ftruncate failed: Not open for writing, or references a socket, not a file.\n");
          break;
        default:     printf("ftruncate failed: Duno why...\n");
          break;
      } /* end switch */
      exit(1);
    } /* end if */
  } /* end if */

  shmAdd = (char *)mmap(NULL,                    // Almost always not used
                        SEGSIZE,                 // Length of the mapped space
                        PROT_READ | PROT_WRITE,  // Access type 
                        MAP_SHARED,              // Write changes to device (see: MAP_PRIVATE)
                        shmFD,                   // FD of the shared memory segment
                        0);                      // Offset into segment.

  /* For more info on the strange return and possible error conditions of mmap(), see the mmap.c example program. */
  if(shmAdd == MAP_FAILED) {
    switch(errno) {
      case EACCES:    printf("mmap failed: The FD was not open for read, or for write with (PROT_WRITE or MAP_SHARED)\n");
        break;
      case EAGAIN:    printf("mmap failed: The mapping could not be locked in memory\n");
        break;
      case EBADF:     printf("mmap failed: The FD not a valid open file descriptor.\n");
        break;
      case EINVAL:    printf("mmap failed: The value of len is zero, addr is not valid, bad combination of args\n");
        break;
      case EMFILE:    printf("mmap failed: The too many regions mapped already\n");
        break;
      case ENODEV:    printf("mmap failed: The FD file type is not supported by mmap().\n");
        break;
      case ENOMEM:    printf("mmap failed: Not enough memory\n");
        break;
      case ENOTSUP:   printf("mmap failed: Options not supported on this platform\n");
        break;
      case ENXIO:     printf("mmap failed: Range [off,off+len) are invalid for the FD, MAP_FIXED & invalid addresses, or FD not accessible\n");
        break;
      case EOVERFLOW: printf("mmap failed: File is too big!\n");
        break;
      default:        printf("mmap failed: Duno why! (errno: %d)\n", errno);
        break;
    } /* end switch */
    exit(1);
  } /* end if */

  if(useMode) {  
    /* The segment already existed, so we print out it's contents and change them.  Just print the printable chars in case the
       segment has binary junk in it... */
    printf("The segment content: \n");
    for(i=0;(i<SEGSIZE)&&(((char *)shmAdd)[i]!=0);i++)
      if(isalpha(((char *)shmAdd)[i]) || isdigit(((char *)shmAdd)[i]))
        printf("%c", ((char *)shmAdd)[i]);
    printf("\n");

    /* Now we change the data to "Goodbye" */
    printf("Change the contents to: 'Goodbye'..\n");
    strcpy(shmAdd, "Goodbye");
  } else {
    /* We copy some stuff into the segment so that we can read it out later... */
    printf("Write 'Hello' into the segment.\n");
    strcpy(shmAdd, "Hello");
  } /* end if/else */

  /* While not required, one should unmap from segments when done with them. */
  if(munmap(shmAdd, SEGSIZE) < 0) {
    switch(errno) {
      case EINVAL:  printf("munmap failed: The address range [addr,addr+len) is invalid.\n"
                           "               munmap failed: The len argument is 0.\n"
                           "               munmap failed: The addr argument is not a multiple of page size.\n");
        break;
      default:      printf("munmap failed: Duno why!  (errno %d).\n", errno);
        break;
    } /* end switch */
    exit(1);
  } /* end if */

  /* One should also close FDs opened with shm_open (again, not necessarily required, but good practice). */
  if(close(shmFD) < 0) {
    switch(errno) {
      case EBADF:   printf("close failed: The FD is not an active descriptor.\n");
        break;
      case EINTR:   printf("close failed: An interrupt was received.\n"); // Should try again... :)
        break;
    } /* end switch */
  } /* end if */

  return 0;
} /* end func main() */
