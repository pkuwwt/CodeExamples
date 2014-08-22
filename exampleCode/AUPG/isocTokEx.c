/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      isocTokEx.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996,2001 by Mitch Richling.  All rights reserved.
   @brief     Typical UNIX applicaiton of ISO C90 strtok() function@EOL
   @Keywords  strtok_r UNIX text delimited split cut awk
   @Std       C90
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)
              - Debian 7.4 + gcc (Debian 4.7.2-5) 4.7.2

   This example has very little to do with UNIX APIs; however, many UNIX systems programmers must deal with text delimited
   databases.  Consider for example: /etc/passwd, /etc/group, and /etc/shadow.  While breaking up such files with tools like perl or
   the shell (cut/awk) is a simple and frequent task, doing so in C via the standard strtok/strtok_r interface is a stumbling block
   for many novice C programmers.

   NOTE: strtok has difficulty with empty tokens, and for this reason many UNIX programmers use the BSD function strsep().  This
   function is available for many UNIX platforms.  An alternative for the C++ programmer is found in the boost library.
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char *pwStr = "root:*:0:0:System Administrator:/var/root:/bin/sh::";
  char  *p, sepChar = ':';                 /* for: DIY */
  char *sep = ":", *pwField, pwStrC[1024]; /* for: strtok_r/strtok */
  char *stCtx;                             /* for: strtok_r*/

  /* If I had a dime for every time I have seen strtok reimplemented, I would be rich!  Don't do it.  Just don't! :) */
  printf("The DIY (do-it-yourself) way:\n");
  printf("'");
  for(p=pwStr; *p!='\0'; p++)
    if(*p == sepChar)
      printf("'\n'");
    else
      printf("%c", *p);
  printf("'\n");

  /* Now we illustrate the traditional use of strtok */
  strcpy(pwStrC, pwStr); /* Can't use a constant string for strtok */
  printf("\nThe strtok way:\n");
  for(pwField=strtok(pwStrC, sep); pwField; pwField=strtok(NULL, sep))
    printf("'%s'\n", pwField);

  /* strtok_r is slightly more complex to call, but it is thread safe. */
  strcpy(pwStrC, pwStr); /* Can't use a constant string for strtok_r */
  printf("\nThe strtok_r way:\n");
  for(pwField=strtok_r(pwStrC, sep, &stCtx); pwField; pwField=strtok_r(NULL,  sep, &stCtx))
    printf("'%s'\n", pwField);
  
  return 0;
} /* end func main */
