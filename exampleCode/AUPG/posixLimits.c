/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      posixLimits.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     UNIX posixLimits function@EOL
   @Keywords  UNIX limits POSIX
   @Std       IEEE Std 1003.1-1988 (POSIX.1) ISOC
   @Tested    
              - MacOS X.3
              - Debian 7.4 + gcc (Debian 4.7.2-5) 4.7.2

   POSIX.1 requires several constants to be present in the limits.h header file.  Be warned, if strict ANSI/ISO C is used, the
   defines below are generally NOT be present!
***********************************************************************************************************************************/

#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <limits.h>             /* uname           POSIX */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

  printf("_POSIX_ARG_MAX ....... Max argument length to exec ............. %llu\n", (unsigned long long int)_POSIX_ARG_MAX        );
  printf("_POSIX_CHILD_MAX ..... Number of children per user ............. %llu\n", (unsigned long long int)_POSIX_CHILD_MAX      );
  printf("_POSIX_LINK_MAX ...... Max links to a file ..................... %llu\n", (unsigned long long int)_POSIX_LINK_MAX       );
  printf("_POSIX_MAX_CANON ..... Bytes on terminal canonical input queue . %llu\n", (unsigned long long int)_POSIX_MAX_CANON      );
  printf("_POSIX_MAX_INPUT ..... terminal input queue length ............. %llu\n", (unsigned long long int)_POSIX_MAX_INPUT      );
  printf("_POSIX_NAME_MAX ...... max filename length ..................... %llu\n", (unsigned long long int)_POSIX_NAME_MAX       );
  printf("_POSIX_NGROUPS_MAX ... Number of secondary groups .............. %llu\n", (unsigned long long int)_POSIX_NGROUPS_MAX    );
  printf("_POSIX_OPEN_MAX ...... Max open files per process .............. %llu\n", (unsigned long long int)_POSIX_OPEN_MAX       );
  printf("_POSIX_PATH_MAX ...... Max pathname length ..................... %llu\n", (unsigned long long int)_POSIX_PATH_MAX       );
  printf("_POSIX_SSIZE_MAX ..... Max for ssize_t type .................... %llu\n", (unsigned long long int)_POSIX_SSIZE_MAX      );
  printf("_POSIX_STREAM_MAX .... Max I/O streams per process ............. %llu\n", (unsigned long long int)_POSIX_STREAM_MAX     );

//  printf("_POSIX_HOST_NAME_MAX . gethostname length max .................. %llu\n", (unsigned long long int)_POSIX_HOST_NAME_MAX  );
//  printf("_POSIX_LOGIN_NAME_MAX  Max login name length ................... %llu\n", (unsigned long long int)_POSIX_LOGIN_NAME_MAX );
//  printf("_POSIX_SYMLINK_MAX ... Max bytes in a symbolic link ............ %llu\n", (unsigned long long int)_POSIX_SYMLINK_MAX    );
//  printf("_POSIX_SYMLOOP_MAX ... Max symlinks in sequence ................ %llu\n", (unsigned long long int)_POSIX_SYMLOOP_MAX    );
//  printf("_POSIX_TTY_NAME_MAX .. Max length of a terminal name ........... %llu\n", (unsigned long long int)_POSIX_TTY_NAME_MAX   );

  printf("_POSIX_TZNAME_MAX .... Max length of a time zone ............... %llu\n", (unsigned long long int)_POSIX_TZNAME_MAX     );

  return 0;
} /* end func main */




