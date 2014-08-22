/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      fqdns.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to get a UNIX host's FQDNS name @EOL
   @Keywords  FQDNS hostname
   @Std       C89 POSIX BSD42
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   Getting the FQDNS from a C program is not as simple as it should be for many UNIX platforms.  The difficulty stems from the fact
   that many UNIX platforms do not return FQDNS from a gethostbyname() call.  The related system call, gethostbyaddr() will always
   return the FQDNS, but you must know the host's IP address to use it.  The solution is to first get the IP address associated with
   a partial host name and then get the FQDNS name associated with it.
***********************************************************************************************************************************/

#include <errno.h>              /* error stf       POSIX */
#include <netdb.h>              /* DNS lookup      ????  */
#include <netinet/in.h>         /* Inet socket     POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <string.h>             /* Strings         C89   */
#include <sys/socket.h>         /* UNIX sockets    POSIX */
#include <sys/stat.h>           /* UNIX stat       POSIX */
#include <sys/types.h>          /* UNIX types      POSIX */
#include <time.h>               /* time            C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <utime.h>              /* utime           POSIX */
#include <arpa/inet.h>          /* inet            ????  */

/**********************************************************************************************************************************/
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 1024
#endif

/**********************************************************************************************************************************/
int main(int argc, const char **argv) {
  struct hostent *hpFrN, *hpFrA;
  char hostName[MAXHOSTNAMELEN];
  char **p;
  struct in_addr inetAdd;

  if(argc > 1) 
    strcpy(hostName, argv[1]);
  else
    if(gethostname(hostName, MAXHOSTNAMELEN)) {
      printf("ERROR: Could not get hostname\n");
      exit(1);
    } /* end if */

  printf("host name to start with: %s\n", hostName);

  hpFrN = gethostbyname(hostName);
  if(hpFrN == NULL) {
    printf("ERROR: Could not look up hostname.\n");
    exit(2);
  } /* end if */
  
  printf("Our host name from gethostbyName: %s\n", hpFrN->h_name);
  printf("Addresses found: \n");
  for (p = hpFrN->h_addr_list; *p != 0; p++) {
      memcpy(&inetAdd.s_addr, *p, sizeof (inetAdd.s_addr));
      printf("  %s\n", inet_ntoa(inetAdd));
  } /* end for */
  printf("Aliases found: \n");
  for (p = hpFrN->h_aliases; *p != 0; p++) {
      printf("  %s\n", *p);
  } /* end for */

  hpFrA = gethostbyaddr((char *)hpFrN->h_addr_list[0], hpFrN->h_length, AF_INET);
  if (hpFrA == NULL) {
    printf("ERROR: Could not lookup host address\n");
    exit (3);
  } /* end if */

  printf("Our host name from gethostbyaddr: %s\n", hpFrA->h_name);
  printf("Addresses found: \n");
  for (p = hpFrA->h_addr_list; *p != 0; p++) {
      memcpy(&inetAdd.s_addr, *p, sizeof (inetAdd.s_addr));
      printf("  %s\n", inet_ntoa(inetAdd));
  } /* end for */
  printf("Aliases found: \n");
  for (p = hpFrA->h_aliases; *p != 0; p++) {
      printf("  %s\n", *p);
  } /* end for */

  return 0;
} /* end func main */

