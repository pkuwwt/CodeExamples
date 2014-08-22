# -*- Mode:Makefile; Coding:us-ascii-unix; fill-column:132 -*-
####################################################################################################################################
##
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2000 by Mitch Richling.  All rights reserved.
# @brief     Build random number generator examples@EOL
# @Keywords  make random number generator example
# @Std       GenericMake
#
# This makefile requires platform specific modification  to work -- fix the variables!
#            

#-----------------------------------------------------------------------------------------------------------------------------------
FC       = gfortran
CC       = gcc
CXX      = g++
FFLAGS   = -Wall
CFLAGS   = -Wall
CXXFLAGS = -Wall

#-----------------------------------------------------------------------------------------------------------------------------------
PRNGINC  = -I/usr/local/include
PRNGLIBP = -L/usr/local/lib
PRNGLIB  = -lprng

#-----------------------------------------------------------------------------------------------------------------------------------
RANLIBFLIBP = -L/usr/local/lib
RANLIBFLIB  = -lRANLIB_hpcFC

#-----------------------------------------------------------------------------------------------------------------------------------
RANLIBCINCP = -I/usr/local/include
RANLIBCLIBP = -L/usr/local/lib
RANLIBCLIB  = -lRANLIBC

#-----------------------------------------------------------------------------------------------------------------------------------
SSLLIBP = -L/usr/lib/
SSLLIB  = -lcrypto -lssl
SSLINCP = -I/usr/include

#-----------------------------------------------------------------------------------------------------------------------------------
# Uncomment to build all when make file changes
SPECDEP=makefile

#-----------------------------------------------------------------------------------------------------------------------------------
# Put targets here
TRG_MINS = minStdRandGenC minStdRandGenF lcgPermPowTwo
TRG_STD  = isocRandEx f90Rand
TRG_BSD  = bsdRandomEx
TRG_SYSV = rand48Ex 
TRG_SSL  = opensslPRandEx
TRG_PRNG = prngEx
TRG_RAN  = ranlibExF ranlibExC randFileEx ranlibExFC

TARGETS  = $(TRG_MINS) $(TRG_STD) $(TRG_SYSV) $(TRG_SSL) $(TRG_RAN) $(TRG_BSD) $(TRG_PRNG) 

#-----------------------------------------------------------------------------------------------------------------------------------
all : $(TARGETS)
	@echo Make Complete

clean :
	rm -rf a.out *~ *.bak *.o $(TARGETS)
	@echo Make Complete

prngEx : prngEx.c $(SPECDEP)
	$(CC) $(CFLAGS) $(PRNGINC) prngEx.c $(PRNGLIBP) $(PRNGLIB) -o prngEx

minStdRandGenC : minStdRandGenC.c $(SPECDEP)
	$(CC) $(CFLAGS) minStdRandGenC.c -o minStdRandGenC

minStdRandGenF : minStdRandGenF.f $(SPECDEP)
	$(FC) $(FFLAGS) minStdRandGenF.f -o minStdRandGenF

lcgPermPowTwo : lcgPermPowTwo.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) lcgPermPowTwo.cpp -o lcgPermPowTwo

ranlibm.mod : ranlibM.f90
	$(FC) $(FFLAGS) -c ranlibM.f90

ranlibExF : ranlibExF.f90 ranlibm.mod $(SPECDEP)
	$(FC) $(FFLAGS) ranlibExF.f90 $(RANLIBFLIBP) $(RANLIBFLIB) -o ranlibExF

ranlibExFC : ranlibExFC.c ranlibF.c $(SPECDEP)
	$(CC) $(CFLAGS) ranlibExFC.c ranlibF.c $(RANLIBFLIBP) $(RANLIBFLIB) -lgfortran -o ranlibExFC

isocRandEx : isocRandEx.c $(SPECDEP)
	$(CC) $(CFLAGS) isocRandEx.c -o isocRandEx

rand48Ex : rand48Ex.c $(SPECDEP)
	$(CC) $(CFLAGS) rand48Ex.c -o rand48Ex

bsdRandomEx : bsdRandomEx.c $(SPECDEP)
	$(CC) $(CFLAGS) bsdRandomEx.c -o bsdRandomEx

opensslPRandEx : opensslPRandEx.c $(SPECDEP)
	$(CC) $(CFLAGS) $(SSLINCP) opensslPRandEx.c $(SSLLIBP) $(SSLLIB) -o opensslPRandEx

ranlibExC : ranlibExC.c $(SPECDEP)
	$(CC) $(CFLAGS)  $(RANLIBCINCP)  ranlibExC.c $(RANLIBCLIBP) $(RANLIBCLIB) -o ranlibExC

randFileEx : randFileEx.c randFileC.h randFileC.c
	$(CC) $(CFLAGS) randFileEx.c randFileC.c -o randFileEx

f90Rand : f90Rand.f90 $(SPECDEP)
	$(FC) $(FFLAGS) f90Rand.f90 -o f90Rand
