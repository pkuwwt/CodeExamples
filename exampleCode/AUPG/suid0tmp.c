/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      suid0tmp.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1994 by Mitch Richling.  All rights reserved.
   @brief     SUID0 template in C@EOL
   @Keywords  suid root template
   @Std       ISOC POSIX
   @Tested    
              - BROKEN: Solaris 2.8 (No paths.h)
              - MacOS X.2
              - Linux (RH 7.3)
              - Ubuntu 10.10
   
   This is an example program intended to illustrate how to develop an SUID0 program.  This code demonstrates a reasonable overall
   structure for an SUID0 program.  Several things are demonstrated:

      -  how to query uid/euid/gid/egid, 
      -  how to log to syslog
      -  How to use asprintf to avoid the buffer overflows, that haunt sprintf
      -  How to sanitize the user environment 
      -  How to make sure open files inherited from the parent process are safe.
      -  How to completely change user identity to increase privileges.
      -  How to permanently surrender SUID privileges.

   NOTE: In general, the "system" function should not be used in an SUID0 program, but it is used here for illustration.
***********************************************************************************************************************************/

/* Define _GNU_SOURCE to get get asprintf */
#define _GNU_SOURCE

/**********************************************************************************************************************************/
#include <errno.h>              /* error stf       POSIX */
#include <paths.h>              /* UNIX Paths      ????  */
#include <pwd.h>                /* UNIX passwd     POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <syslog.h>             /* UNIX syslog     UNIX  */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <utime.h>              /* utime           POSIX */
#include <string.h>             /* Strings         C89   */

/**********************************************************************************************************************************/
/** define the uid we will set the binary to. */
#define TARGETEUID 0
/** define the gid we will set the binary to. */
#define TARGETEGID 0

/**********************************************************************************************************************************/
#define PROGNAME "suid0tmp"
#define WHOAMIBIN "/usr/bin/whoami"

/**********************************************************************************************************************************/
int secureEnv(char **keepEnv, char **setEnv, char **defEnv);
void secureEnvOrExit(char **keepEnv, char **setEnv, char **defEnv);
void secureFDsOrExit();
int secureFDs();

/**********************************************************************************************************************************/
extern char **environ;

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  uid_t theUid;
  uid_t theEuid;
  gid_t theGid;
  char *strBuf;

  /* Close all FDs over 2, and make sure all FDs under 3 are open. Exit if we can't fix the FDs.*/
  secureFDsOrExit();

  /* Discover who the user is, and store the info.  At this point the euid/egid should be set by the SUID/SGID bits of the binary.
     The uid/gid should be the uid/gid of the parent process -- the real user. */
  theUid  = getuid();
  theEuid = geteuid();
  theGid  = getgid();  // getegid();

  /* Log the activity.  Note that we do not use sprintf as it is not safe.  Note also that we don't use snprintf as it is not
     portable (it exists on many platforms including BSD and Linux).  asprintf is also not portable, but it is part of GCC which is
     available on most platforms. */
  if(asprintf(&strBuf, "%s(%s): run by uid=%ld", PROGNAME, argv[0], (long)theUid) <= 0) {
    printf("ERROR: asprinf() failure..\n");
    exit(1);
  } /* end if */
  syslog(LOG_NOTICE | LOG_AUTH, "%s", strBuf);

  /* If we didn't actually change user it could be that the filesystem doesn't support SUID bits, or that the bits were not set on
     the binary.  Whatever the reason, we exit now. */
  if(theEuid != TARGETEUID) {
    printf("ERROR: SUID not granted.\n");
    exit(1);
  } /* end if */

  /* If we didn't actually change group.  Could be that the filesystem doesn't support SUID bits, or that the bits were not set on
     the binary. */
  if(theEuid != TARGETEGID) {
    printf("ERROR: SGID not granted.\n");
    exit(1);
  } /* end if */

  /* Here we do whatever we want to with the current IDs */
  printf("1 uid: %ld euid: %ld gid: %ld egid: %ld\n", 
         (long)getuid(), (long)geteuid(), 
         (long)getgid(), (long)getegid());
  system(WHOAMIBIN);

  /* Now we make sure we really look like the target by setting the uid/gid as well.  We can really only do this if the target UID
     is 0(root). */
  setuid(TARGETEUID);
  setgid(TARGETEGID);

  printf("2 uid: %ld euid: %ld gid: %ld egid: %ld\n", 
         (long)getuid(), (long)geteuid(), 
         (long)getgid(), (long)getegid());
  system(WHOAMIBIN);

  /* Now we become the user -- or at least the uid/gid of the parent process.  Note we set the gid/egid first.  If we set the
     uid/euid first, we would not have privilege to change the gid/egid Again, note that if the TARGET is not 0(root) we can't do
     this part. */
  setgid(theGid);
  setegid(theGid);
  setuid(theUid);
  seteuid(theUid);

  printf("3 uid: %ld euid: %ld gid: %ld egid: %ld\n",  (long)getuid(), (long)geteuid(),  (long)getgid(), (long)getegid());
  system(WHOAMIBIN);

  /* Finish up */
  return (0);
} /* end func main */

/**********************************************************************************************************************************/
void secureFDsOrExit() {
  int secureFDreturn;

  /* Close all FDs over 2, and make sure all FDs under 3 are open. */
  secureFDreturn = secureFDs();
  if(secureFDreturn == 1) {
    printf("ERROR: Could not determine file descriptor table size.\n");
    exit(1);
  } else if( (secureFDreturn >= 2) && (secureFDreturn <= 4) ) {
    printf("ERROR: Unknown stat error (FD: %d).\n", secureFDreturn-2);
    exit(1);
  } else if( (secureFDreturn >= 5) && (secureFDreturn <= 7) ) {
    printf("ERROR: Could not reopen FD %d.\n", secureFDreturn-5);
    exit(1);
  } else if( (secureFDreturn >= 8) && (secureFDreturn <= 10) ) {
    printf("ERROR: Wrong FD result from reopen of %d\n", secureFDreturn-8);
    exit(1);
  } /* end if/else */
} /* end func secureFDsOrExit */

/**********************************************************************************************************************************/
/* Closes all file descriptors greater than 2.  Makes sure that stdin, stdout, and stderr are open.  If they are not, then it opens
   them.

   Returns: -1  Everything worked (reopens required) 
             0  Everything worked
             1  Error using getdtablesize
             2  Unknown stat error (wasn't EBADF) stdin
             3  Unknown stat error (wasn't EBADF) stdout
             4  Unknown stat error (wasn't EBADF) stderr
             5  If the reopen fails               stdin
             6  If the reopen fails               stdout
             7  If the reopen fails               stderr
             8  Wrong FD result from reopen       stdin
             9  Wrong FD result from reopen       stdout
            10  Wrong FD result from reopen       stderr
 */
int secureFDs() {
  int daReturn = 0;
  int numFDs; 
  struct stat s;
  FILE *f;
  int fd;

  if( (numFDs = getdtablesize()) < 0) {
    return 1;
  } /* end if */

  /* Close all file descriptors except stdin, stdout, and stderr (0, 1, and 2) */
  for(fd=3; fd<numFDs; fd++) 
    close(fd);

  /* Make sure that FD 0, 1, and 2 are open.  If they are not, then open them and make sure stdin, stdout, and stderr are associated
     with FD 0, 1, and 2. */
  for(fd=0; fd<3; fd++) {
    if(fstat(fd, &s) < 0) { /* It has a problem, or it is closed. */
      daReturn = -1;
      if(errno != EBADF) { /* EBADF == closed, most of the time. */
        return 2+fd;
      } /* end if */
      /* fd is closed if we get here. */
      f = NULL;
      if(fd == 0) {
        f = freopen(_PATH_DEVNULL, "rb", stdin);
      } else if(fd == 1) {
        f = freopen(_PATH_DEVNULL, "wb", stdout);
      } else if(fd == 2) {
        f = freopen(_PATH_DEVNULL, "wb", stderr);
      } /* end if/else */
      if(f == NULL) { /* The freopen failed. */
        return 5+fd;
      } /* end if */
      if(fd != fileno(f)) { /* Somehow didn't get the right FD! */
        return 8+fd;
      } /* end if */
    } /* end if */
  } /* end for */
  return daReturn;
} /* end secureFDs */

/**********************************************************************************************************************************/
/* This function calls secureEnv() and exits if anything goes wrong.  The arguments are the same as what secureEnv() requires, and
   they are directly passed to secureEnv(). */
void secureEnvOrExit(char **keepEnv, char **setEnv, char **defEnv) {
  int secureEnvReturn;
  secureEnvReturn = secureEnv(keepEnv, setEnv, defEnv);
  if       (secureEnvReturn == 1)  {
    printf("ERROR: Could not zap environment variable.\n");
  } else if(secureEnvReturn == 2)  {
    printf("ERROR: Insanely long environment variable.\n");
  } else if(secureEnvReturn == 3)  {
    printf("ERROR: Could not set environment variable.\n");
  } else if(secureEnvReturn == 4)  {
    printf("ERROR: Call to getpwuid failed.\n");
  } else if(secureEnvReturn == 6)  {
    printf("ERROR: Could not set PATH to default value.\n");
  } else if(secureEnvReturn == 7)  {
    printf("ERROR: Could not set TMPDIR to default value.\n");
  } else if(secureEnvReturn == 8)  {
    printf("ERROR: Could not set HOME to default value.\n");
  } else if(secureEnvReturn == 9)  {
    printf("ERROR: Could not set LOGNAME to default value.\n");
  } else if(secureEnvReturn == 10) {
    printf("ERROR: Could not set SHELL to default value.\n");
  } /* end if/else */

  if(secureEnvReturn > 0)
    exit(1);
} /* end secureEnvReturn */

/**********************************************************************************************************************************/
/* Defaults:  keepEnv==NULL => keep nothing
              setEnv==NULL  => set nothing
              defEnv==NULL  => set PATH, SHELL, LOGNAME, and HOME
   Returns: 0 => Everything worked
            1 => Malformed element of envdir (no '=' char)
            2 => An environment variable name was very long (>512b)
            3 => Could not set environment variable (from setEnv)
            4 => Could not get passwd info (for defEnv)
            5 => Not used
            6 => Could not set PATH to default value (from defEnv)
            7 => Could not set TMPDIR to default value (from defEnv)
            8 => Could not set HOME to default value (from defEnv)
            9 => Could not set LOGNAME to default value (from defEnv)
           10 => Could not set SHELL to default value (from defEnv)
*/
int secureEnv(char **keepEnv, char **setEnv, char **defEnv) {
  char *DkeepEnv[] = {NULL
                     };
  char *DsetEnv[] = {NULL
                    };
  char *DdefEnv[] = {"PATH",
                     "SHELL",
                     "LOGNAME",
                     "HOME",
                     NULL
                    };
  char **cp, **pp, *eqLoc;
  int keep;
  char chrBuf[1048];
  struct passwd *pwEnt = NULL;
  uid_t theUID;

  /* Take care of default values for the arguments. */
  if(keepEnv == NULL) 
    keepEnv=DkeepEnv;
  if(setEnv == NULL)  
    setEnv=DsetEnv;
  if(defEnv == NULL)  
    defEnv=DdefEnv;

  /* Delete all variables not listed in the keepEnv variable. */
  for(cp=environ; *cp != NULL; cp++){
    keep=0;
    for(pp=keepEnv; (*pp != NULL) && (!keep); pp++) {
      if(strstr(*cp, *pp) == *cp) { /* keepEnv line must start the environ line */
        if(strlen(*cp) > strlen(*pp)) { /* environ line must be longer keepEnv line */
          if((*cp)[strlen(*pp)] == '=') { /* Make sure it is a complete match */
            keep = 1;
          } /* end if */
        } /* end if */
      } /* end if */
    } /* end for */
    if(keep) {
    } else {
      eqLoc = strchr(*cp, '=');
      if(eqLoc == NULL) {
        return 1;
      } /* end if */
      if((eqLoc - *cp) > 512) {
        return 2;
      } /* end if */
      strncpy(chrBuf, *cp, (eqLoc-*cp));
      chrBuf[eqLoc-*cp] = '\0';
      unsetenv(chrBuf);
      cp=environ; /* Start over. */
    } /* end if/else */
  } /* end for */

  /* Set all the variables given by setEnv. */
  for(pp=setEnv; *pp != NULL ; pp++) {
    if(putenv(*pp) < 0) {
      return 3;
    } /* end if */
  } /* end for */

  /* Now set all the variables in defEnv to the default values specified by various system parameters. Supported are: PATH, HOME,
     LOGNAME, SHELL, TMPDIR. */
  for(pp=defEnv; *pp != NULL ; pp++) {
    if         (strcmp(*pp, "PATH") == 0) {
      if(setenv("PATH", _PATH_STDPATH, 1) < 0)
        return 6;
    } else if(strcmp(*pp, "TMPDIR") == 0) {
      if(setenv("TMPDIR", _PATH_TMP, 1) < 0)
        return 7;
    } else { /* Must be something from the passwd DB. */
      theUID = getuid();
      if((pwEnt = getpwuid(theUID)) == NULL) {
        return 4;
      } /* end if */
      if(strcmp(*pp, "HOME") == 0) {
        if(setenv("HOME", pwEnt->pw_dir, 1) < 0) 
          return 8;
      } else if(strcmp(*pp, "LOGNAME") == 0) {
        if(setenv("LOGNAME", pwEnt->pw_name, 1) < 0)
          return 9;
      } else if(strcmp(*pp, "SHELL") == 0) {
        if(setenv("SHELL", pwEnt->pw_shell, 1) < 0)
          return 10;
      } /* end if/else */
    } /* end if/else */
  } /* end for */
  return 0;
} /* end func secureEnv */
