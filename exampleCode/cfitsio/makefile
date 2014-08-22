##
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1998 by Mitch Richling.  All rights reserved.
# @brief     Build some CFITSIO/FITSIO example programs@EOL
# @Keywords  fits cfitsio fitsio example
# @Std       GenericMake

# Flags for CFITSIO
#CFITSIOL  = -lcfitsio
#CFITSIOLP = -L/opt/local/lib/
#CFITSIOIP = -I/opt/local/include/
CFITSIOL  = -lcfitsio
CFITSIOLP = -L/usr/local/lib/
CFITSIOIP = -I/usr/local/include/

# Compilers
FC       = g77
CC       = gcc
CXX      = g++

# Compiler flags
FFLAGS   = -Wall -Wimplicit
CFLAGS   = -Wall $(CFITSIOIP)
CXXFLAGS = -Wall $(CFITSIOIP)

# Linker flags for Linux & MacOS X
LDFLAGS  = $(CFITSIOLP) $(CFITSIOL) -lm

# Linker flags for Solaris
#LDFLAGS  = $(CFITSIOLP) $(CFITSIOL) -lm -lsocket -lnsl 

# Uncomment to build all when make file changes
SPECDEP=makefile

# Put targets here
TARGETS = fits2tga dumpFitsHeader

all : $(TARGETS)
	@echo Make Complete

clean :
	rm -rf a.out *~ *.bak $(TARGETS)
	@echo Make Complete

fits2tga : fits2tga.c fitsUtil.c fitsUtil.h $(SPECDEP)
	$(CC) -Wall $(CFLAGS) fits2tga.c fitsUtil.c $(LDFLAGS) -o fits2tga

dumpFitsHeader : dumpFitsHeader.c fitsUtil.c fitsUtil.h $(SPECDEP)
	$(CC) $(CFLAGS) dumpFitsHeader.c fitsUtil.c $(LDFLAGS) -o dumpFitsHeader
