/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      env.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1995 by Mitch Richling.  All rights reserved.
   @brief     How to access environment variables@EOL
   @Keywords  UNIX shell environment variable
   @Std       ISOC
   @Tested    
              - BROKEN: Solaris 2.8 (no setenv/unsetenv)
              - MacOS X.2
              - Linux (RH 7.3)

   This C program is intended to illustrate how one can query and modify environment variables.  get env(), putenv(), unsetenv(),
   and setenv() are all demonstrated.  A safe way to append to the PATH variable is also demonstrated(this is often done wrong);
***********************************************************************************************************************************/

#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char nullStr[] = "";
  char *envSHELL, *envFOO, *envPATH, *newPATH;

  /* This is a slick way to query the environment, and return a null string if the environment variable doesn't exist.  This method
     won't let you know the difference between an environment variable that is the empty string and a variable that doesn't
     exist. */
  if((envSHELL = getenv("SHELL")) == NULL)
    envSHELL = nullStr;
  printf("SHELL=%s\n", envSHELL);

  /* This is a more precise way to query a variable. */
  envFOO = getenv("FOO");
  if(envFOO == NULL) {
    printf("The variable FOO is not defined.\n");
  } /* end if */

  /* You can clear a variable like this: */
  printf("Clearing SHELL\n");
  unsetenv("SHELL");
  if((envSHELL = getenv("SHELL")) == NULL)
    envSHELL = nullStr;
  printf("SHELL=%s\n", envSHELL);

  /* You can Set an environment variable like this. */
  printf("Setting SHELL=/bin/foo\n");
  if(setenv("SHELL", "/bin/foo", 0))
    printf("Couldn't set SHELL\n");
  if((envSHELL = getenv("SHELL")) == NULL)
    envSHELL = nullStr;
  printf("SHELL=%s\n", envSHELL);

  /* Note we can's set it now with 0 as the last argument now because the variable now exists. */
  printf("Trying SHELL=/bin/foobar with no overwrite flag set.\n");
  if(setenv("SHELL", "/bin/foobar", 0))
    printf("Couldn't set SHELL\n");
  if((envSHELL = getenv("SHELL")) == NULL)
    envSHELL = nullStr;
  printf("SHELL=%s\n", envSHELL);
  printf("Trying SHELL=/bin/foobar with the overwrite flag set.\n");
  if(setenv("SHELL", "/bin/foobar", 1))
    printf("Couldn't set SHELL\n");
  if((envSHELL = getenv("SHELL")) == NULL)
    envSHELL = nullStr;
  printf("SHELL=%s\n", envSHELL);

  /* The putenv() system call is equivalent to setenv(key, val, 1) */
  printf("Trying SHELL=/bin/bar with the putenv() system call.\n");
  if(putenv("SHELL=/bin/bar"))
    printf("Couldn't set SHELL\n");
  if((envSHELL = getenv("SHELL")) == NULL)
    envSHELL = nullStr;
  printf("SHELL=%s\n", envSHELL);

  /* Here we demonstrate appending to the PATH variable in a safe way that has no limitations except the ability to allocate RAM. */
  if((envPATH = getenv("PATH")) == NULL)
    envPATH = nullStr;
  printf("PATH=%s\n", envPATH);
  if(envPATH[0] == '\0') {
    newPATH = "/usr/local/bin";
  } else {
    newPATH = (char *)malloc((strlen(envPATH) + strlen(":/usr/local/bin") + 1) * sizeof(char));
    strcpy(newPATH, envPATH);
    strcat(newPATH, ":/usr/local/bin");
  } /* end if/else */
  if(newPATH == NULL) {
    printf("malloc() error.\n");
    return 1;
  } /* end if */
  setenv("PATH", newPATH, 1);
  if((envPATH = getenv("PATH")) == NULL)
    envPATH = nullStr;
  printf("PATH=%s\n", envPATH);

  return 0;
} /* end func main */
