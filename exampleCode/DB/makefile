
# Fix for local needs
CC       = gcc
CXX      = g++
CFLAGS   = -O3 -Wall
CXXFLAGS = -O3 -Wall

# Specify BDB location -- some sites will have this in /usr/local
# making this completely unnecessary.  On BSDs it is best to put it
# someplace else as the system ships with a 1.8x vantage version of
# sleepycat BDB.  Note we link to the static libdb library to avoid
# runtime linker problems -- BDB is small and nicely embedded.
BDB_HOME = /usr/lib

# What kind of sicko would have gdbm someplace not on the default
# include and library paths?  I duno, but just in case you can set it
# here!
GDBM_HOME = /usr/local
#GDBM_HOME = /usr
#GDBM_HOME = /apps/free/gdbm/1.8.3

# If the gdbm libs are in a normal place, the following are not
# required -- the -R is for runtime and -L for link time.
#GDBM_LP   = -L$(GDBM_HOME)/lib
#GDBM_RP   = -R$(GDBM_HOME)/lib

# ndbm libs.  For solaris, this should be BLANK.
NDBM_LIB = -ldbm
NDBM_LIB = 

# Uncomment to build all when make file changes
SPECDEP=makefile

TARGETS = rdNDBM mkNDBM mkGDBM rdGDBM rdBerkeleyDB mkBerkeleyDB 

all : ${TARGETS}
	@echo Make Complete

clean :
	rm -f ${TARGETS} ${SOLARISS} *~ *.bak a.out ndbmTest.db berkeleydbTest.db gdbmTest.gdb
	@echo Make Complete

mkNDBM : mkNDBM.c $(SPECDEP)
	$(CC) $(CFLAGS) mkNDBM.c $(NDBM_LIB) -o mkNDBM

mkGDBM : mkGDBM.c $(SPECDEP)
	$(CC) $(CFLAGS) -I$(GDBM_HOME)/include mkGDBM.c $(GDBM_LP) $(GDBM_RP) -lgdbm -o mkGDBM

rdGDBM : rdGDBM.c $(SPECDEP)
	$(CC) $(CFLAGS) -I$(GDBM_HOME)/include rdGDBM.c $(GDBM_LP) $(GDBM_RP) -lgdbm -o rdGDBM

rdNDBM : rdNDBM.c $(SPECDEP)
	$(CC) $(CFLAGS) rdNDBM.c $(NDBM_LIB) -o rdNDBM

mkBerkeleyDB : mkBerkeleyDB.c $(SPECDEP)
	$(CC) $(CFLAGS) -I$(BDB_HOME)/include mkBerkeleyDB.c  $(BDB_HOME)/lib/libdb.a -o mkBerkeleyDB

rdBerkeleyDB : rdBerkeleyDB.c $(SPECDEP)
	$(CC) $(CFLAGS) -I$(BDB_HOME)/include rdBerkeleyDB.c $(BDB_HOME)/lib/libdb.a -o rdBerkeleyDB
