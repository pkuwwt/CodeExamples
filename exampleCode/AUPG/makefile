

CC       = gcc
CXX      = g++
CFLAGS   = -Wall
CXXFLAGS = -Wall

#CC       = icc
#CXX      = icpc
#CFLAGS   = -Wall
#CXXFLAGS = -Wall -std=c++0x 


TARGETS =  arguments getoptP IOerrors dirDepth
TARGETS += dirThreadCPP98 dirThreadCPP11
TARGETS += dirDepthC dirRec dirFastC readdir
TARGETS += env environ fileData fileIO mmap sparse
TARGETS += forkbunny forknwait suid0tmp rssu
TARGETS += uname passwd uideuid getgrXXX getgroups getpwXXX gidegid group hosts fqdns 
TARGETS += inetSocC inetSocS kill paths param sysconf popenDemo
TARGETS += ptCond ptJoin ptMutex
TARGETS += regex_example select_poll sigaction signal sleep
TARGETS += unixSocC unixSocS fileStuff 
TARGETS += shmMakeAndUseRT shmRemoveRT shmMake semMakeRT shmUse shmRemove
TARGETS += curDraw curHello curHelloMV curHelloMVW curHelloW curInput curColor 
TARGETS += termSizeWatch posixLimits
TARGETS += isocLimits rlimit xdrFilter isocTokEx pcre_example
TARGETS += rlEX rlEXC rlEXH

SOLARISS = passwdshadow solaris_ps 

all : ${TARGETS}
	@echo Make Complete

alls : ${SOLARISS}
	@echo Make Complete

clean :
	rm -f ${TARGETS} ${SOLARISS} *~ *.bak* a.out ndbmTest.db
	@echo Make Complete

uname : uname.c

rlimit : rlimit.c

posixLimits : posixLimits.c

xdrFilter : xdrFilter.c

isocLimits : isocLimits.c

getoptP : getoptP.c

dirDepth : dirDepth.c

dirDepthC : dirDepthC.cpp
	$(CXX) $(CXXFLAGS) -std=c++98 dirDepthC.cpp -o dirDepthC

dirFastC : dirFastC.cpp
	$(CXX) $(CXXFLAGS) -std=c++98 dirFastC.cpp -o dirFastC

dirRec : dirRec.c

dirThreadCPP98 : dirThreadCPP98.cpp mtUtils/mtUtils.c
	$(CXX) $(CXXFLAGS) -std=c++98 -lpthread -I./mtUtils dirThreadCPP98.cpp mtUtils/mtUtils.c -o dirThreadCPP98

dirThreadCPP11 : dirThreadCPP11.cpp
	$(CXX) $(CXXFLAGS) -std=c++11 dirThreadCPP11.cpp -D_GLIBCXX_USE_NANOSLEEP -lpthread -o dirThreadCPP11

IOerrors : IOerrors.c

arguments : arguments.c

env : env.c

environ : environ.c

fileData : fileData.c

fileIO : fileIO.c

forkbunny : forkbunny.c

forknwait : forknwait.c

fqdns : fqdns.c

getgrXXX : getgrXXX.c

getgroups : getgroups.c

getpwXXX : getpwXXX.c

gidegid : gidegid.c

group : group.c

hosts : hosts.c

inetSocC : inetSocC.c

inetSocS : inetSocS.c

kill : kill.c

passwd : passwd.c

passwdshadow : passwdshadow.c

paths : paths.c

param : param.c

sysconf : sysconf.c

popenDemo : popenDemo.c

ptCond : ptCond.c mtUtils/mtUtils.c
	$(CC) $(CFLAGS) -lpthread -I./mtUtils ptCond.c mtUtils/mtUtils.c -o ptCond

ptJoin : ptJoin.c mtUtils/mtUtils.c
	$(CC) $(CFLAGS) -lpthread -I./mtUtils ptJoin.c mtUtils/mtUtils.c -o ptJoin

ptMutex : ptMutex.c  mtUtils/mtUtils.c
	$(CC) $(CFLAGS) -lpthread -I./mtUtils ptMutex.c mtUtils/mtUtils.c -o ptMutex

readdir : readdir.c

regex_example : regex_example.c

isocTokEx : isocTokEx.c

rssu : rssu.c

select_poll : select_poll.c

sigaction : sigaction.c

signal : signal.c

sleep : sleep.c

solaris_ps : solaris_ps.c

sparse : sparse.c

suid0tmp : suid0tmp.c

uideuid : uideuid.c

unixSocC : unixSocC.c

unixSocS : unixSocS.c

unixSocS : unixSocS.c

fileStuff : fileStuff.c

semMakeRT : semMakeRT.c

shmMake : shmMake.c

shmMakeAndUseRT : shmMakeAndUseRT.c
	$(CC) $(CFLAGS) -lrt shmMakeAndUseRT.c -o shmMakeAndUseRT

shmRemoveRT : shmRemoveRT.c
	$(CC) $(CFLAGS) -lrt shmRemoveRT.c -o shmRemoveRT

shmUse : shmUse.c

shmRemove : shmRemove.c

utime : utime.c

mmap : mmap.c

termSizeWatch : termSizeWatch.c

CURHOME = /usr/local
#CURHOME = /apps/free/ncurses/5.4/include
#CURINCP = -I$(CURHOME)/include
#CURLIBP = -L$(CURHOME)/lib
CURLIB  = -lncurses

curHelloW : curHelloW.c
	$(CC) $(CFLAGS) $(CURINCP) $(CURLIBP) curHelloW.c $(CURLIB) -o curHelloW

curHelloMV : curHelloMV.c
	$(CC) $(CFLAGS) $(CURINCP) $(CURLIBP) curHelloMV.c $(CURLIB) -o curHelloMV

curHelloMVW : curHelloMVW.c
	$(CC) $(CFLAGS) $(CURINCP) $(CURLIBP) curHelloMVW.c $(CURLIB) -o curHelloMVW

curHello : curHello.c
	$(CC) $(CFLAGS) $(CURINCP) $(CURLIBP) curHello.c $(CURLIB) -o curHello

curInput : curInput.c
	$(CC) $(CFLAGS) $(CURINCP) $(CURLIBP) curInput.c $(CURLIB) -o curInput

curDraw : curDraw.c
	$(CC) $(CFLAGS) $(CURINCP) $(CURLIBP) curDraw.c $(CURLIB) -o curDraw

curColor : curColor.c
	$(CC) $(CFLAGS) $(CURINCP) $(CURLIB) curColor.c $(CURLIB) -o curColor

## MacOS X readline
#RLLIB   = -lreadline
#RLLIBP  = -L/usr/lib
#RLINCP  = -I/usr/include

## Typical GNU readline installed via DarwinPorts
RLLIB   = -lreadline
RLLIBP  = -L/opt/local/lib
RLINCP  = -I/opt/local/include

rlEX : rlEX.c
	$(CC) $(CFLAGS) $(RLINCP) rlEX.c $(RLLIBP) $(RLLIB) -o rlEX

rlEXH : rlEXH.c
	$(CC) $(CFLAGS) $(RLINCP) rlEXH.c $(RLLIBP) $(RLLIB) -o rlEXH

rlEXC : rlEXC.c
	$(CC) $(CFLAGS) $(RLINCP) rlEXC.c $(RLLIBP) $(RLLIB) -o rlEXC

# Typical GNU readline installed via DarwinPorts
PCRELIB   = -lpcre
PCRELIBP  = -L/opt/local/lib
PCREINCP  = -I/opt/local/include

pcre_example : pcre_example.c
	$(CC) $(CFLAGS) $(PCREINCP) pcre_example.c $(PCRELIBP) $(PCRELIB) -o pcre_example
