/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      rlimit.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
   @brief     How to access rlimit values@EOL
   @Keywords  UNIX rlimit ulimit limit
   @Std       XSI
   @Tested    
              - MacOS X.4

   This C program is intended to illustrate how one can query and modify resource limits (see ulimit & limit).  Not all systems
   support all limits, so we have lots of #ifdef's in this code. :(
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */
#include <sys/resource.h>       /* limits          SUS   */

/**********************************************************************************************************************************/
void printLimit(int theLimitNum, char *theLimitStr);

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

#ifdef RLIMIT_CPU
  printLimit(RLIMIT_CPU, "RLIMIT_CPU (cpu time)");
#endif
#ifdef RLIMIT_FSIZE
  printLimit(RLIMIT_FSIZE, "RLIMIT_FSIZE (file size)");
#endif
#ifdef RLIMIT_DATA
  printLimit(RLIMIT_DATA, "RLIMIT_DATA (data seg size)");
#endif
#ifdef RLIMIT_STACK
  printLimit(RLIMIT_STACK, "RLIMIT_STACK (stack size)");
#endif
#ifdef RLIMIT_CORE
  printLimit(RLIMIT_CORE, "RLIMIT_CORE (core file size)");
#endif
#ifdef RLIMIT_AS
  printLimit(RLIMIT_AS, "RLIMIT_AS (resident set size)");
#endif
#ifdef RLIMIT_RSS
  printLimit(RLIMIT_RSS, "RLIMIT_RSS (resident set size)");
#endif
#ifdef RLIMIT_MEMLOCK
  printLimit(RLIMIT_MEMLOCK, "RLIMIT_MEMLOCK (locked memory)");
#endif
#ifdef RLIMIT_NPROC
  printLimit(RLIMIT_NPROC, "RLIMIT_NPROC (user processes)");
#endif
#ifdef RLIMIT_NOFILE
  printLimit(RLIMIT_NOFILE, "RLIMIT_NOFILE (open files)");
#endif

  return 0;
} /* end func main */

/**********************************************************************************************************************************/
void printLimit(int theLimitNum, char *theLimitStr) {
  struct rlimit rlimitStruct;
  if(getrlimit(theLimitNum, &rlimitStruct) == -1) {
    printf("ERROR: getrlimit failure %s\n", theLimitStr);
    return;
  } /* end if */
  printf("%-32s   ", theLimitStr);
  if(rlimitStruct.rlim_cur == RLIM_INFINITY) 
    printf("cur: %15s   ", "INFINITY");
  else
    printf("cur: %15lld   ", (long long)rlimitStruct.rlim_cur);
  if(rlimitStruct.rlim_max == RLIM_INFINITY) 
    printf("max: %15s   ", "INFINITY");
  else
    printf("max: %15lld   ", (long long)rlimitStruct.rlim_max);
  printf("\n");

} /* end func printLimit */
