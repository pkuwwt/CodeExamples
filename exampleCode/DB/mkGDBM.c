/** 
   @file      mkGDBM.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
   @brief     How to read an GDBM DB@EOL
   @Keywords  UNIX database dbm ndbm gdbm 
   @Std       ISOC POSIX UNIX98 BSD4.3 SYSV3

   @Tested    
              - Solaris 2.8     (NOT)
              - MacOS X.2
*/

#include <gdbm.h>               /* gdbm header     GNU   */
#include <fcntl.h>              /* UNIX file ctrl  UNIX  */
#include <string.h>             /* Strings         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <unistd.h>             /* UNIX std stf    POSIX */
#include <errno.h>              /* error stf       POSIX */

#define NUMELE 4

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  int i, dbRet;
  GDBM_FILE myDB;
  datum daKey, daVal;
  char *keys[NUMELE] = {"foo", "bar", "foobar", "foo"};
  char *vals[NUMELE] = {"FOO", "BAR", "FOOBAR", "FOOZAM"};

  /* Open the database (create) */
  /* Arg 3: GDBM_NEWDB == alwasy make new one, GDBM_WRCREAT == open for write, creat if required, 
            GDBM_READER, GDBM_WRITER 
            For write ones, can or GDB_SYNC, GDBM_NOLOCK */
  myDB = gdbm_open("gdbmTest.gdb", 512, GDBM_WRCREAT | GDBM_NOLOCK | GDBM_SYNC, 0664, NULL);
  if(myDB == NULL) {
    printf("ERROR: Could not open the DB file.\n");
    exit(1);
  } else {
    printf("DB created.\n");
    printf("DB opened.\n");
  } /* end if */
    
  /* Put stuff in the DB... */
  for(i=0; i<NUMELE; i++) {
    daKey.dptr = keys[i];
    daKey.dsize = strlen(keys[i])+1;
    daVal.dptr = vals[i];
    daVal.dsize = strlen(vals[i])+1;
    dbRet = gdbm_store(myDB, daKey, daVal, GDBM_INSERT);  // GDBM_REPLACE for replace behaviour
    switch(dbRet) {
    case 0:
      printf("Store: '%s' ==> '%s'\n", (char *)daKey.dptr, (char *)daVal.dptr); 
      break;
    case 1: // Only can happen with DBM_INSERT 
      printf("Could not store '%s' because it was already in the DB\n", (char *)daKey.dptr); 
      break;
    default:
      printf("ERROR: Could not insert item (%s,%s). \n", (char *)daKey.dptr, (char *)daVal.dptr);
      exit(1);
    } /* end switch */
  } /* end for */ 
 
  /* Lookup the second one.. */
  daKey.dptr = keys[1];
  daKey.dsize = strlen(keys[1])+1;
  daVal = gdbm_fetch(myDB, daKey);
  if(daVal.dptr == NULL) {
    printf("ERROR: Could not look up %s\n", (char *)daKey.dptr);
  } else {
    printf("Got record: '%s' ==> '%s'\n", (char *)daKey.dptr, (char *)daVal.dptr);
    /* We must free() space allocated by fetch -- not doing so is a very common source of memory leaks. */
    free(daVal.dptr);
  } /* end if */
 
  /* Delete an element */
  daKey.dptr = keys[1];
  daKey.dsize = strlen(keys[1])+1;
  if(gdbm_delete(myDB, daKey) < 0) {
    printf("ERROR: Could not delete item with key %s\n", (char *)daKey.dptr);
  } else {
    printf("Deleted: '%s'\n", (char *)daKey.dptr);
  } /* end if */

  /* With GDBM you can check for a key without fetching the value... */
  daKey.dptr = keys[1];
  daKey.dsize = strlen(keys[1])+1;
  dbRet = gdbm_exists(myDB, daKey);
  if(dbRet) {
    printf("Key exists: '%s'\n", (char *)daKey.dptr);
  } else {
    printf("Key is not in DB: '%s'\n", (char *)daKey.dptr);
  } /* end if */
 
   /* Close the DB (flush everything to the file) */
  gdbm_close(myDB);
  printf("DB closed... Bye!\n");

  return 0;
} /* end func main */
