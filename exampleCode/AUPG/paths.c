/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      paths.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     How to use paths.h@EOL
   @Keywords  paths.h unix
   @Std       UNKNOWN
   @Tested    
              - BROKEN: Solaris 2.8 (no paths.h)
              - MacOS X.2
              - Linux (RH 7.3)
   
   This is an example program intended to illustrate how to make use of the paths.h header available on most UNIX systems.  I do not
   know the standards pedigree of this include file at this time, but it is quite widely available.  Not all of the defines used
   below are defined on all UNIX systems.  I have selected a set of the most commonly found defines.
***********************************************************************************************************************************/

#include <stdio.h>              /* I/O lib         C89   */
#include <paths.h>              /* UNIX Paths      ????  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {

  printf("_PATH_DEFPATH  %s\n", _PATH_DEFPATH  );
  printf("_PATH_STDPATH  %s\n", _PATH_STDPATH  );
  printf("_PATH_BSHELL   %s\n", _PATH_BSHELL   );
  printf("_PATH_DEVNULL  %s\n", _PATH_DEVNULL  );
  printf("_PATH_MAN      %s\n", _PATH_MAN      );
  printf("_PATH_MEM      %s\n", _PATH_MEM      );
  printf("_PATH_TTY      %s\n", _PATH_TTY      );
  printf("_PATH_VI       %s\n", _PATH_VI       );
  printf("_PATH_DEV      %s\n", _PATH_DEV      );
  printf("_PATH_TMP      %s\n", _PATH_TMP      );
#ifndef __CYGWIN__
  printf("_PATH_CONSOLE  %s\n", _PATH_CONSOLE  );
  printf("_PATH_MAILDIR  %s\n", _PATH_MAILDIR  );
  printf("_PATH_NOLOGIN  %s\n", _PATH_NOLOGIN  );
  printf("_PATH_SENDMAIL %s\n", _PATH_SENDMAIL );
  printf("_PATH_SHELLS   %s\n", _PATH_SHELLS   );
  printf("_PATH_VARTMP   %s\n", _PATH_VARTMP   );
#endif
#ifdef BSD
  printf("_PATH_LOCALE   %s\n", _PATH_LOCALE   );
  printf("_PATH_RSH      %s\n", _PATH_RSH      );
#endif

  return 0;
} /* end func main */
