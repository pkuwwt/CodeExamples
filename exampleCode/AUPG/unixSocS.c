/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      unixSocS.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX domain sockets (server application)@EOL
   @Keywords  UNIX domain sockets
   @Std       ISOC POSIX SYSV4 BSD4.3
   @Tested    
              - BROKEN: Solaris 2.8 (No paths.h)
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to write a very simple UNIX domain sockets server.  Std Comp:
   Typical of SVID 4 and BSD 4.3.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/socket.h>         /* UNIX sockets    POSIX */
#include <sys/un.h>             /* UNIX address    UNIX  */
#include <errno.h>              /* error stf       POSIX */
#include <string.h>             /* Strings         C89   */
#include <stdio.h>              /* I/O lib         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */

/**********************************************************************************************************************************/
int main() {
  int theSocket;
  struct sockaddr_un theSocketName;
  int aSocket;
  char rcvdData[1024];
  int numRcvdBytes;
  socklen_t theAddrLen; /* Some platforms use an 'int' for this type. */

  theSocket = socket(AF_UNIX, SOCK_STREAM, 0);

  if(theSocket < 0) {
    perror("ERROR: unxDomSoc: Open error");
    exit(1);
  } /* end if */

  theSocketName.sun_family = AF_UNIX;
  strcpy(theSocketName.sun_path, "/tmp/theSocketFile");
  theAddrLen = sizeof(theSocketName) - sizeof(theSocketName.sun_path) + strlen(theSocketName.sun_path);
  unlink("/tmp/theSocketFile");

  if(bind(theSocket, (struct sockaddr *)&theSocketName, theAddrLen) < 0) {
    perror("ERROR: unxDomSoc: Bind error");
    exit(1);
  } /* end if */

  if(listen(theSocket, 2) < 0) {
    perror("ERROR: unxDomSoc: Listen error");
    exit(1);
  } /*end if */

  aSocket = accept(theSocket, (struct sockaddr *)&theSocketName, &theAddrLen);
  if(aSocket < 0) {
    perror("ERROR: unxDomSoc: Accept error");
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
  } /* end if/else */

  close(aSocket);
  close(theSocket);

  return 0;
} /* end func main */
