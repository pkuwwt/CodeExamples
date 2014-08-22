/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      hosts.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX host queries@EOL
   @Keywords  UNIX host information
   @Std       ISOC POSIX SYSV3 BSD4.3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to query the name of the current host, and how to query a UNIX host's
   resolver which may, in turn, query any one of several data sources includeing /etc/hosts, DNS, NIS, NIS+, NetInfo, LDAP, and
   Active Directory.  In today's world, this is normally just DNS.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/socket.h>         /* UNIX sockets    POSIX */
#include <string.h>             /* Strings         C89   */
#include <netdb.h>              /* DNS lookup      ????  */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdio.h>              /* I/O lib         C89   */
#include <netinet/in.h>         /* Inet socket     POSIX */
#include <arpa/inet.h>          /* inet            ????  */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  char hostName[512];
  struct hostent *theHostEnt;
  char **p, **q;
  struct in_addr aAddress;

  /* Use host on command line, or use the current hostname if no arguments are given. */
  if(argc > 1)
    strncpy(hostName, argv[1], sizeof(hostName) - 1);
  else if(gethostname(hostName, sizeof(hostName)) < 0) {
    perror("ERROR: hosts: Couldn't get host name");
    exit(1);
  } /* end if */
  printf("Host name: %s\n", hostName);

  /* Now lets lookup this host's information */
  if((theHostEnt = gethostbyname(hostName)) == NULL) {
    perror("ERROR: hosts: Host unknown");
    exit(1);
  } /* end if */

  printf("Official name: %s\n", theHostEnt->h_name);

  /* This if is something that is not frequently seen in real code.  This is because EVERYBODY uses IP, and many simply assume that
     the addresses they get will be IP addresses. */
  if(theHostEnt->h_addrtype == AF_INET) {
    printf("Using IP addresses.\n");
    printf("IP addresses: ");
    for(p = theHostEnt->h_addr_list; *p != 0; p++) {
      memcpy(&aAddress.s_addr, *p, sizeof(aAddress.s_addr));
      printf(" %s", inet_ntoa(aAddress));
    } /* end for */
    printf("\n");
  } else
    printf("Not using IP addresses.\n");

  printf("aliases: ");
  for(q = theHostEnt->h_aliases; *q != 0; q++)
    printf(" %s", *q);
  printf("\n");

  return 0;

} /* end func main */
