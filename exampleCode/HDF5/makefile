# -*- Mode:Makefile; Coding:us-ascii-unix; fill-column:132 -*-
####################################################################################################################################
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
# @brief     Build HDF5 example programs.@EOL
# @Keywords  HDF5 example
# @Std       GenericMake
#            
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# Fix for local needs
FC       = g77
CC       = gcc
CXX      = g++
FFLAGS   = -Wall -Wimplicit
CFLAGS   = -O3 -Wall
CXXFLAGS = -O3 -Wall

#-----------------------------------------------------------------------------------------------------------------------------------
# HDF5 stuff
HDF5INCPATH = -I/opt/local/include
HDF5LIBPATH = -L/opt/local/lib
HDF5LIB     = -lhdf5 -lhdf5_hl

#-----------------------------------------------------------------------------------------------------------------------------------
# Uncomment to build all when make file changes
SPECDEP=makefile

#-----------------------------------------------------------------------------------------------------------------------------------
# Put targets here
TARGETS = hdf5NewFile hdf5ReadModifyWrite mjrHDF5.o hdf5Query hdf5NewFileEC hdf5Compound hdf5Hyperslab hdf5ImageRead hdf5ImageWrite hdf5Table hdf5LiteNewFile

#-----------------------------------------------------------------------------------------------------------------------------------
all : $(TARGETS)
	@echo Make Complete

clean :
	rm -rf a.out *~ *.bak *.bak? *.h5 $(TARGETS)
	@echo Make Complete

mjrHDF5.o : mjrHDF5.c mjrHDF5.h
	$(CC) $(CFLAGS) $(HDF5INCPATH) -c mjrHDF5.c -o mjrHDF5.o

hdf5ReadModifyWrite : hdf5ReadModifyWrite.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5ReadModifyWrite.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5ReadModifyWrite

hdf5NewFile : hdf5NewFile.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5NewFile.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5NewFile

hdf5NewFileEC : hdf5NewFileEC.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5NewFileEC.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5NewFileEC

hdf5Query : hdf5Query.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5Query.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5Query

hdf5Compound : hdf5Compound.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5Compound.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5Compound

hdf5Hyperslab : hdf5Hyperslab.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5Hyperslab.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5Hyperslab

hdf5ImageRead : hdf5ImageRead.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5ImageRead.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5ImageRead

hdf5ImageWrite : hdf5ImageWrite.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5ImageWrite.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5ImageWrite

hdf5Table : hdf5Table.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5Table.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5Table

hdf5LiteNewFile : hdf5LiteNewFile.c mjrHDF5.o $(SPECDEP)
	$(CC) $(CFLAGS) $(HDF5INCPATH) hdf5LiteNewFile.c mjrHDF5.o $(HDF5LIBPATH) $(HDF5LIB) -o hdf5LiteNewFile
