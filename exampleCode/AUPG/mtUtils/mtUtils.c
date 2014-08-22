 /**
   @file      mtUtils.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     Basic pthread programming@EOL
   @Keywords  UNIX pthreads POSIX mutex
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3

              This source file us used to provide several useful
              definitions for multi-threaded programs. C++ support is
              not complete.

   @Build     
              - Solaris 2.8: use -lpthread -lrt

   @Tested    
              - Solaris 2.8
              - MacOS X.2        
              - Linux (RH 7.3)
*/

#include <pthread.h>            /* threads         POSIX */
#include <sched.h>              /* threads         POSIX */

#if __cplusplus
#include <cstdio>               /* ISOC I/O        C++   */
#include <cstdlib>              /* ISOC std lib    C++   */
#include <cstdarg>              /* Variable args   C++   */
#else
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <stdarg.h>             /* Variable args   ISOC  */
#endif /* __cplusplus */

#include "mtUtils.h"

/* ************************************************************************** */
pthread_mutex_t console_mutex = PTHREAD_MUTEX_INITIALIZER;

/* ************************************************************************** */
int mallocNsetInt(int **toSet, int value) {
  if((*toSet = (int *)malloc(sizeof(int))) != NULL) {
    **toSet = value;
    return 0;
  } else {
    return 1;
  } /* end if/else */
} /* end func mallocNsetInt */

/* ************************************************************************** */
int mtPrintf(const char* format, ...) {
  va_list arg;
  int theReturn;

  va_start(arg, format);
  pthread_mutex_lock(&console_mutex);
  theReturn = vprintf(format, arg);
  pthread_mutex_unlock(&console_mutex);
  va_end(arg);
  
  return theReturn;
} /* end func mtPrintf */
