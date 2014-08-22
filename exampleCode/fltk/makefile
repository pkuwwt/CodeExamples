##
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1997 by Mitch Richling.  All rights reserved.
# @brief     @EOL
# @Keywords  
# @Std       GNUmake BSDmake SYSVmake GenericMake
#
#            Notes here
#            


FLUID = /Applications/DarwinPorts/fltk/fluid
FLUID = /usr/bin/fluid

# Uncomment to build all when make file changes
#SPECDEP=makefile

# Put targets here
TARGETS = minimalFLTK mainWinCallback simpleButton simpleButtonOO glutWithFltk glutWithFltkInput simpleFluid

all : $(TARGETS)
	@echo Make Complete

clean :
	rm -rf a.out *~ *.bak $(TARGETS)
	rm -rf simpleFluid.cpp simpleFluid.h
	@echo Make Complete

minimalFLTK : minimalFLTK.cpp
	fltk-config --compile minimalFLTK.cpp

mainWinCallback : mainWinCallback.cpp
	fltk-config --compile mainWinCallback.cpp

simpleButton : simpleButton.cpp
	fltk-config  --compile simpleButton.cpp

simpleButtonOO : simpleButtonOO.cpp
	fltk-config  --compile simpleButtonOO.cpp

glutWithFltk : glutWithFltk.cpp
	fltk-config --use-glut --use-gl --compile glutWithFltk.cpp

glutWithFltkInput : glutWithFltkInput.cpp
	fltk-config --use-glut --use-gl --compile glutWithFltkInput.cpp

# NOTE: May need to add -lGLU to the above command line

simpleFluid.cpp : simpleFluid.fl
	$(FLUID) -c -o simpleFluid.cpp -h simpleFluid.h simpleFluid.fl

simpleFluid : simpleFluid.cpp simpleFluid.h 
	fltk-config --compile simpleFluid.cpp
