/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      inetSocS.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX internet domain sockets (server side)@EOL
   @Keywords  UNIX Internet domain sockets client
   @Std       ISOC POSIX SYSV4 BSD4.3
   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to write a very simple Internet domain sockets server.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/socket.h>         /* UNIX sockets    POSIX */
#include <netinet/in.h>         /* Inet socket     POSIX */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <string.h>             /* Strings         C89   */

/**********************************************************************************************************************************/
int main(int argc, char *argv[]) {
  int theSocket;
  struct sockaddr_in theSocketName;
  int aSocket;
  char rcvdData[1024];
  int numRcvdBytes;
  socklen_t theAddrLen; /* Some platforms use an 'int' for this type. */
  int theAddr;

  theSocket = socket(AF_INET, SOCK_STREAM, 0);

  if(theSocket < 0) {
    perror("ERROR: inetDomSoc: Open error");
    exit(1);
  } /* end if */

  theSocketName.sin_family = AF_INET;
  theSocketName.sin_port = htons(12345);
  /* This constant will let us listen on ALL network interfaces on a host. */
  theAddr = INADDR_ANY;
  /* You can NOT use a cast and assignment here.  Data alignment may not work in your favor.  memcpy() is a good way to get the data
     into this variable. */
  memcpy(&theSocketName.sin_addr, &theAddr, sizeof(theAddr));
  theAddrLen = sizeof(theSocketName);

  /* Note that below this line the code is the same as the example program for UNIX domain sockets.  This is a nice feature of the
     socket interface. */

  if(bind(theSocket, (struct sockaddr *)&theSocketName, theAddrLen) < 0) {
    perror("ERROR: inetDomSoc: Bind error");
    exit(1);
  } /* end if */

  if(listen(theSocket, 2) < 0) {
    perror("ERROR: inetDomSoc: Listen error");
    exit(1);
  } /* end if */

  aSocket = accept(theSocket, (struct sockaddr *)&theSocketName, &theAddrLen);
  if(aSocket < 0) {
    perror("ERROR: inetDomSoc: Accept error");
    exit(1);
  } /* end if */

  printf("Client Connected...\n");
  while((numRcvdBytes = recv(aSocket, rcvdData, 100, 0)) > 0) {
    printf("Received %d bytes from client:\n", numRcvdBytes);
    write(1, rcvdData, numRcvdBytes);
  } /* end while */

  /* Test to see if we had an error, or simply no more data. */
  if(numRcvdBytes < 0) {
    /* The error type should be checked.  Some errors are not a reason to quit (EINTER for example) */
    perror("ERROR: unxDomSoc: Recv error");
    exit(1);
  } else {
    printf("EOF\n");
  } /* end if */

  close(aSocket);
  close(theSocket);

  return 0;
} /* end func main */
