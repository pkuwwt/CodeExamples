/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      param.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 2004 by Mitch Richling.  All rights reserved.
   @brief     How to use paths.h@EOL
   @Keywords  paths.h unix
   @Std       UNKNOWN
   @Tested    
              - MacOS X.3
   
   This is an example program intended to illustrate how to make use of the param.h header available on some UNIX systems (BSD
   variants).
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <sys/param.h>          /* UNIX Params     ????  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

#ifdef MAXINTERP     
  printf("MAXINTERP        max interpreter file name length   %d\n", (int)MAXINTERP);
#else
  printf("MAXINTERP        is not supported on this platform\n");
#endif
#ifdef MAXLOGNAME    
  printf("MAXLOGNAME       max login name length              %d\n", (int)MAXLOGNAME);
#else
  printf("MAXLOGNAME       is not supported on this platform\n");
#endif
#ifdef MAXUPRC       
  printf("MAXUPRC          max simultaneous processes         %d\n", (int)MAXUPRC);
#else
  printf("MAXUPRC          is not supported on this platform\n");
#endif
#ifdef NCARGS        
  printf("NCARGS           max bytes for an exec function     %d\n", (int)NCARGS);
#else
  printf("NCARGS           is not supported on this platform\n");
#endif
#ifdef NOGROUP       
  printf("NOGROUP          marker for empty group set member  %d\n", (int)NOGROUP);
#else
  printf("MAXINTERP        is not supported on this platform\n");
#endif
#ifdef MAXDOMNAMELEN 
  printf("MAXDOMNAMELEN    maximum domain name length         %d\n", (int)MAXDOMNAMELEN);
#else
  printf("MAXINTERP        is not supported on this platform\n");
#endif
#ifdef NGROUPS       
  printf("NGROUPS          max number groups                  %d\n", (int)NGROUPS);
#else
  printf("MAXINTERP        is not supported on this platform\n");
#endif
#ifdef NOFILE        
  printf("NOFILE           default max open files per process %d\n", (int)NOFILE);
#else
  printf("NOFILE           is not supported on this platform\n");
#endif
#ifdef MAXHOSTNAMELEN
  printf("MAXHOSTNAMELEN   max hostname size                  %d\n", (int)MAXHOSTNAMELEN);
#else
  printf("MAXHOSTNAMELEN   is not supported on this platform\n");
#endif

  return 0;
} /* end func main */
