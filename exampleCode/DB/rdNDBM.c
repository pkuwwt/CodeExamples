/** 
   @file      rdNDBM.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     How to open and traverse an NDBM database@EOL
   @Keywords  UNIX database dbm ndbm gdbm 
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3

   @Tested    
              - Solaris 2.8
              - MacOS X.2

*/

#include <fcntl.h>              /* UNIX file ctrl  UNIX  */
#include <ndbm.h>               /* ndbm header     BSD   */
#include <string.h>             /* Strings         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <errno.h>              /* error stf       POSIX */

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  int i;
  DBM *db;
  datum daKey, daVal;

  /* Open the database (create) */
  db = dbm_open("ndbmTest", O_RDWR, 0660);
  if(db == NULL) {
    printf("ERROR: Could not open the DB file.  Error number: %d.\n", errno);
    exit(1);
  } else {
    printf("DB handle created.\n");
    printf("DB file opened.\n");
  }

  /* At this point you can add and remove things from the DB just as in mkNDBM. */
 
  /* Traverse the entire DB and lookup each key. */
  printf("All the records in the DB:\n");
  for(i=1,daKey=dbm_firstkey(db); daKey.dptr!=NULL; daKey=dbm_nextkey(db),i++) {
    daVal = dbm_fetch(db, daKey);
    if(daVal.dptr == NULL) {
      printf("ERROR: Could not look up %s\n", (char *)daKey.dptr);
    } else {
      printf("  Record(%d): '%s' ==> '%s'\n", i, (char *)daKey.dptr, (char *)daVal.dptr);
    } /* end if/else */
  } /* end for */
  printf("Found %d records\n", i-1);

  /* Close the DB (flush everything to the file) */
  dbm_close(db);
  printf("DB closed... Bye!\n");

  return 0;
} /* end func main */
