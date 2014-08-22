##
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1999 by Mitch Richling.  All rights reserved.
# @brief     Build the MPI example programs.@EOL
# @Keywords  MPI examples
# @Std       GenericMake

# Depending upon which MPI implementation is used, the following may
# need to be adjusted.
MPICC  = mpicc
MPICXX = mpic++
MPIFC  = mpif77

# Uncomment to build all when make file changes
#SPECDEP=makefile

TARGETS = initStuffC helloC helloF90 helloF77 helloCPP reduceC sendRecvC sendRecvErrC barrierC groupsC

all : $(TARGETS)
	@echo Make Complete

helloC : helloC.c $(SPECDEP)
	$(MPICC) helloC.c -o helloC

helloCPP : helloCPP.cpp $(SPECDEP)
	$(MPICXX) helloCPP.cpp -o helloCPP

helloF90 : helloF90.f90 $(SPECDEP)
	$(MPIFC) helloF90.f90 -o helloF90

helloF77 : helloF77.f $(SPECDEP)
	$(MPIFC) helloF77.f -o helloF77

initStuffC : initStuffC.c $(SPECDEP)
	$(MPICC) initStuffC.c -o initStuffC

reduceC : reduceC.c $(SPECDEP)
	$(MPICC) reduceC.c -o reduceC

sendRecvC : sendRecvC.c $(SPECDEP)
	$(MPICC) sendRecvC.c -o sendRecvC

sendRecvErrC : sendRecvErrC.c $(SPECDEP)
	$(MPICC) sendRecvErrC.c -o sendRecvErrC

barrierC : barrierC.c $(SPECDEP)
	$(MPICC) barrierC.c -o barrierC

groupsC : groupsC.c $(SPECDEP)
	$(MPICC) groupsC.c -o groupsC

clean :
	rm -rf a.out *~ *.bak $(TARGETS) hostnames p4pg.hostnames
	@echo Make Complete
