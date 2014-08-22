/* -*- Mode:C; Coding:us-ascii-unix; fill-column:132 -*- */
/**********************************************************************************************************************************/
/**
   @file      unixSocC.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1996 by Mitch Richling.  All rights reserved.
   @brief     UNIX domain sockets (client application)@EOL
   @Keywords  UNIX domain sockets client
   @Std       ISOC POSIX SYSV4 BSD4.3
   @Tested    
              - BROKEN: Solaris 2.8 (No paths.h)
              - MacOS X.2
              - Linux (RH 7.3)

   This is an example program intended to illustrate how to write a very simple UNIX domain sockets client.
***********************************************************************************************************************************/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <sys/socket.h>         /* UNIX sockets    POSIX */
#include <sys/un.h>             /* UNIX address    UNIX  */
#include <string.h>             /* Strings         C89   */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <stdlib.h>             /* Standard Lib    C89   */
#include <stdio.h>              /* I/O lib         C89   */

/**********************************************************************************************************************************/
int main() {
  int theSocket;
  struct sockaddr_un theSocketName;
  int theAddrLen;
  char sendData[1024];
  int numReadBytes, numSentBytes;

  theSocket = socket(AF_UNIX, SOCK_STREAM, 0);
  if(theSocket < 0) {
    perror("ERROR: unxDomSoc: Open error");
    exit(1);
  } /* end if */

  theSocketName.sun_family = AF_UNIX;
  strcpy(theSocketName.sun_path, "/tmp/theSocketFile");
  theAddrLen = sizeof(theSocketName) - sizeof(theSocketName.sun_path) + strlen(theSocketName.sun_path);

  if(connect(theSocket, (struct sockaddr *)&theSocketName, theAddrLen) < 0) {
    perror("ERROR: unxDomSoc: Connect error");
    exit(1);
  } /* end if */

  while((numReadBytes = read(0, sendData, 100)) > 0)
    if((numSentBytes = send(theSocket, sendData, numReadBytes, 0)) < 0) {
      perror("ERROR: unxDomSoc: Send Error");
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
