##
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
# @brief     Build example NetCDF programs.@EOL
# @Keywords  make netcdf example
# @Std       GenericMake
#


# Fix for local needs
FC       = g77
CC       = gcc
CXX      = g++
FFLAGS   = -Wall -Wimplicit
CFLAGS   = -Wall -O3
CXXFLAGS = -Wall

# NetCDF stuff
NCINCPATH = -I/opt/local/include
NCLIBPATH = -L/opt/local/lib
NCLIB     = -lnetcdf

# Uncomment to build all when make file changes
SPECDEP=makefile

# Put targets here
TARGETS = ncNewFile ncHeaderDump ncReadFile

all : $(TARGETS)
	@echo Make Complete

ncNewFile : ncNewFile.c mjrNetCDF.o $(SPECDEP)
	$(CC) $(CFLAGS) $(NCINCPATH) ncNewFile.c mjrNetCDF.o -o ncNewFile $(NCLIBPATH) $(NCLIB)

ncReadFile : ncReadFile.c mjrNetCDF.o $(SPECDEP)
	$(CC) $(CFLAGS) $(NCINCPATH) ncReadFile.c mjrNetCDF.o -o ncReadFile $(NCLIBPATH) $(NCLIB)

ncHeaderDump : ncHeaderDump.c mjrNetCDF.o $(SPECDEP)
	$(CC) $(CFLAGS) $(NCINCPATH) ncHeaderDump.c mjrNetCDF.o -o ncHeaderDump $(NCLIBPATH) $(NCLIB)

mjrNetCDF.o : mjrNetCDF.c mjrNetCDF.h $(SPECDEP)
	$(CC) $(CFLAGS) $(NCINCPATH) -c mjrNetCDF.c -o mjrNetCDF.o

clean :
	rm -rf a.out *~ *.bak *.bak? *.o $(TARGETS)
	@echo Make Complete
