/** 
   @file      rdGDBM.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     How to open and traverse an GDBM database@EOL
   @Keywords  UNIX database dbm gdbm gdbm 
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3

   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RH 7.3)

*/

#include <fcntl.h>              /* UNIX file ctrl  UNIX  */
#include <gdbm.h>               /* gdbm header     GNU   */
#include <string.h>             /* Strings         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <errno.h>              /* error stf       POSIX */

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  int i;
  GDBM_FILE myDB;
  datum daKey, daVal, tKey;

  /* Open the database (create) */
  myDB = gdbm_open("gdbmTest.gdb", 512, GDBM_READER, 0664, NULL);
  if(myDB == NULL) {
    printf("ERROR: Could not open the DB file.  Error number: %d.\n", errno);
    exit(1);
  } else {
    printf("DB handle created.\n");
    printf("DB file opened.\n");
  }

  /* At this point you can add and remove things from the DB just as in mkGDBM. */
 
  /* Traverse the entire DB and lookup each key.  Note the free() in the for loop 
     increment code!*/
  printf("All the records in the DB:\n");
  for(i=1,daKey=gdbm_firstkey(myDB); daKey.dptr!=NULL; tKey=daKey,daKey=gdbm_nextkey(myDB, daKey),free(tKey.dptr),i++) {
    daVal = gdbm_fetch(myDB, daKey);
    if(daVal.dptr == NULL) {
      printf("ERROR: Could not look up %s\n", (char *)daKey.dptr);
    } else {
      printf("  Record(%d): '%s' ==> '%s'\n", i, (char *)daKey.dptr, (char *)daVal.dptr);
      free(daVal.dptr);  // Never forget to free stuff you get from fetch!
    } /* end if/else */
  } /* end for */
  printf("Found %d records\n", i-1);

  /* Close the DB (flush everything to the file) */
  gdbm_close(myDB);
  printf("DB closed... Bye!\n");

  return 0;
} /* end func main */
