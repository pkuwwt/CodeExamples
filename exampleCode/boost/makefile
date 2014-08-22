##
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 2003 by Mitch Richling.  All rights reserved.
# @brief     Build a few boost examples.@EOL
# @Std       GNUmake

# Fix for local needs
CXX      = g++
CXXFLAGS = -Wall

# Uncomment to build all when make file changes
SPECDEP=makefile

# Put targets here
TARGETS = tokenExample regexExample1 boostRandEx format threadSimple threadBind threadFunc threadCond intervalA splitWithSplitAlgo splitWithTokenizer

all : $(TARGETS)
	@echo Make Complete

clean :
	rm -rf a.out *~ *.bak $(TARGETS)
	@echo Make Complete

#BOOSTIP = -I/opt/local/include
#BOOSTLP = -L/opt/local/lib

tokenExample : tokenExample.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) tokenExample.cpp -o tokenExample

regexExample1 : regexExample1.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) regexExample1.cpp -o regexExample1 $(BOOSTLP) -lboost_regex

boostRandEx : boostRandEx.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) boostRandEx.cpp -o boostRandEx $(BOOSTLP) -lboost_regex

format : format.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) format.cpp -o format

threadBind : threadBind.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) threadBind.cpp -o threadBind $(BOOSTLP) -lboost_thread

threadSimple : threadSimple.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) threadSimple.cpp -o threadSimple $(BOOSTLP) -lboost_thread

threadFunc : threadFunc.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) threadFunc.cpp -o threadFunc $(BOOSTLP) -lboost_thread

threadCond : threadCond.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) threadCond.cpp -o threadCond $(BOOSTLP) -lboost_thread

intervalA : intervalA.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) intervalA.cpp -o intervalA

splitWithSplitAlgo : splitWithSplitAlgo.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) splitWithSplitAlgo.cpp -o splitWithSplitAlgo

splitWithTokenizer : splitWithTokenizer.cpp $(SPECDEP)
	$(CXX) $(CXXFLAGS) $(BOOSTIP) splitWithTokenizer.cpp -o splitWithTokenizer
