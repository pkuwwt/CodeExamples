/**
   @file      rdBerkeleyDB.c
   @author    Mitch Richling <http://www.mitchr.me/>
   @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
   @brief     How to read a Berkely DB.@EOL
   @Keywords  berkely DB dbm ndbm gdbm
   @Std       C99              

   @Tested    
              - Solaris 2.8
              - MacOS X.2
              - Linux (RHEL)
*/

#include <sys/types.h>          /* UNIX types      POSIX */
#include <stdio.h>              /* I/O lib         ISOC  */
#include <stdlib.h>             /* Standard Lib    ISOC  */
#include <string.h>             /* Strings         ISOC  */
#include <db.h>                 /* dbm header      ????  */

#define DATABASE "access.db"

void zeroDBT(DBT *dbt);
void closeDB(DB *dbp);

int main(int argc, char *argv[]) {
  DB *myDB; 
  int dbRet, i, maxErr;
  DBT key, value;
  DBC *myDBcursor;

  /* Create the DB */
  if ((dbRet = db_create(&myDB, NULL, 0)) != 0) { // Open failure
    fprintf(stderr, "db_create: %s\n", db_strerror(dbRet)); 
    exit (1); 
  } else {
    printf("DB handle created.\n");
  }

  /* Associate DB with a file (create a btree) -- don't create it!*/
  if ((dbRet = myDB->open(myDB, NULL, "berkeleydbTest.db", NULL, DB_BTREE, 0, 0)) != 0) { 
    myDB->err(myDB, dbRet, "berkeleydbTest.db"); 
    exit(1); // Should close DB now, but we don't cause we are just gonna exit
  } else {
    printf("DB file opened.\n");
  } 

  /* Get a cursor into the DB */
  dbRet = myDB->cursor(myDB, NULL, &myDBcursor, 0);
  switch(dbRet) {
  case 0: 
    printf("DB cursor initialized.\n");
    break;
  default:
    myDB->err(myDB, dbRet, "DB->cursor");
    exit(1); // Should close DB now, but we don't cause we are just gonna exit
  }

  /* Traverse DB and print what we find.. */
  printf("All the records in the DB:\n");
  i=0;
  maxErr=100;
  zeroDBT(&key);
  zeroDBT(&value);
  while((dbRet = myDBcursor->c_get(myDBcursor, &key, &value, DB_NEXT)) == 0) {
    i++;
    switch(dbRet) {
    case 0:
      printf("  Record(%d): '%s' ==> '%s'\n", i, (char *)key.data, (char *)value.data);
      break;
    case DB_NOTFOUND:
      printf("  Hmmm.. Record not found..\n");
      maxErr--;
      if(maxErr <= 0) {
        printf("  Too many errors.  I'm gonna give up now!\n");
        exit(1);
      }
      break;
    default:
      myDB->err(myDB, dbRet, "  DBcursor->get");
      exit(1);
    }
  }
  printf("Found %d records\n", i);

  /* We should always close our DB -- even before we exit.. */
  closeDB(myDB);
  printf("DB closed... Bye!\n");

  exit(1);
}

void closeDB(DB *dbp) {
  int dbRet;
  dbRet = dbp->close(dbp, 0);
  switch(dbRet) {
  case 0:
    break;
  default:
    printf("Fail: Could not close the db...\n");
  }
}

void zeroDBT(DBT *dbt) {
  memset(dbt, 0, sizeof(DBT));  
}
