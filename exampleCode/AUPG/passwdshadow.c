/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      passwdshadow.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX password and shadow password degrees@EOL
   @Keywords  UNIX password shadow passwd getpwuid
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3
   @Tested    
              - Solaris 2.8
              - BROKEN: MacOS X.2 (no shadow API)
              - Linux (RH 7.3)
              - Debian 7.4 + gcc (Debian 4.7.2-5) 4.7.2

   This is an example program intended to illustrate how to query most UNIX versions for password and shadow password information.
   This has been tested on Solaris and Linux.
***********************************************************************************************************************************/

#include <pwd.h>                /* UNIX passwd     POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <shadow.h>             /* UNIX shadow DB  SYSV  */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  struct passwd *pwEnt;
  struct spwd *spEnt;
  char pwSalt[3];
  uid_t theUid, theEuid;

  /* Figure out who the user is running this thing. */
  theUid = getuid();
  theEuid = geteuid();
  printf("uid: %ld  euid: %ld\n", (long)theUid, (long)theEuid);

  /* Get the password record for the user. */
  pwEnt = getpwuid(theUid);
  if(pwEnt == NULL) {
    printf("ERROR: getpwuid.\n");
    exit(1);
  } /* end if */
  printf("pw_name:   %s\n", pwEnt->pw_name);
  printf("pw_passwd: %s\n", pwEnt->pw_passwd);
  printf("pw_uid:    %ld\n", (long)pwEnt->pw_uid);
  printf("pw_gid:    %ld\n", (long)pwEnt->pw_gid);
  printf("pw_gecos:  %s\n", pwEnt->pw_gecos);
  printf("pw_dir:    %s\n", pwEnt->pw_dir);
  printf("pw_shell:  %s\n", pwEnt->pw_shell);

  /* Get the shadow information for this user. */
  spEnt = getspnam(pwEnt->pw_name);
  if(spEnt == NULL) {
    printf("ERROR: getspnam.\n");
    exit(1);
  } /* end if */
  printf("sp_namp:    %s\n", spEnt->sp_namp);
  printf("sp_pwdp:    %s\n", spEnt->sp_pwdp);
  printf("sp_lastchg: %ld\n", (long)spEnt->sp_lstchg);
  printf("sp_min:     %ld\n", (long)spEnt->sp_min);
  printf("sp_max:     %ld\n", (long)spEnt->sp_max);
  printf("sp_warn:    %ld\n", (long)spEnt->sp_warn);
  printf("sp_intact:  %ld\n", (long)spEnt->sp_inact);
  printf("sp_expire:  %ld\n", (long)spEnt->sp_expire);
  printf("sp_flag:    %ld\n", (long)spEnt->sp_flag);

  /* Extract the salt from the encrypted password. */
  pwSalt[0] = (spEnt->sp_pwdp)[0];
  pwSalt[1] = (spEnt->sp_pwdp)[1];
  pwSalt[2] = '\0';
  printf("Salt: %s\n", pwSalt);

  return 0;
} /* end func main */
