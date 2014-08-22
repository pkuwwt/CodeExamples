/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      inetSocC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX internet domain sockets (client side)@EOL
   @Keywords  UNIX Internet domain sockets client
   @Std       ISOC POSIX SYSV4 BSD4.3
   @Tested
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to write a very simple Internet domain sockets client.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/socket.h>         /* UNIX sockets    POSIX */
#include <netinet/in.h>         /* Inet socket     POSIX */
#include <string.h>             /* Strings         C89   */
#include <netdb.h>              /* DNS lookup      ????  */
#include <stdio.h>              /* I/O lib         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int theSocket;
  struct sockaddr_in theSocketName;
  int theAddrLen;
  char sendData[1024];
  int numReadBytes, numSentBytes;
  struct hostent *theHostEnt;

  if(argc < 2) {
    fprintf(stderr, "ERROR: inetDomSoc: Too few arguments.\n");
    fprintf(stderr, "  use: inetDomSocClnt <hostname>\n");
    exit(1);
  } /* end if */

  theSocket = socket(AF_INET, SOCK_STREAM, 0);
  if(theSocket < 0) {
    perror("ERROR: inetDomSoc: Open error");
    exit(1);
  } /* end if */

  /* We need to construct the address to connect to. */
  if((theHostEnt = gethostbyname(argv[1])) == NULL) {
    perror("ERROR: inetDomSoc: Host unknown");
    exit(1);
  } /* end if */

  theSocketName.sin_family = AF_INET;
  theSocketName.sin_port = htons(12345);
  memcpy(&theSocketName.sin_addr, theHostEnt->h_addr_list[0], theHostEnt->h_length);
  theAddrLen = sizeof(theSocketName);

  /* Note that this program from this point down is the same as the UNIX domain socket example program.  This is a nice feature of
     the socket interface. */

  if(connect(theSocket, (struct sockaddr *)&theSocketName, theAddrLen) < 0) {
    perror("ERROR: inetDomSoc: Connect error");
    exit(1);
  } /* end if */

  while((numReadBytes = read(0, sendData, 100)) > 0)
    if((numSentBytes = send(theSocket, sendData, numReadBytes, 0)) < 0) {
      perror("ERROR: inetDomSoc: Send Error");
      exit(1);
    } else {
      printf("Sent %d of %d bytes\n", numSentBytes, numReadBytes);
      /* We should check to make sure we sent all the bytes we wanted to.  If we didn't, we should retry the send.  See the
         IOerrors.c example for more detail. */
      if(numSentBytes < numReadBytes)
        printf("We didn't send all the bytes we wanted to.  Should retry..\n");
    } /* end if/else */

  close(theSocket);

  return 0;
} /* end func main */
