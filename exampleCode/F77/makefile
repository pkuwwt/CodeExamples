
# Fix for local needs
FC       = gfortran
FFLAGS   = -Wall -std=legacy

# The targets
TARGETS = arith array do goto hello ifth io fileio subs coms statf func types loopsg datas param savey loopml implml impl fmtio chars intfil

all : $(TARGETS)

clean :
	rm -f *~ *.bak a.out outf $(TARGETS)

loopns: loopns.f
	$(FC) $(FFLAGS) loopns.f -o loopns

loopsg: loopsg.f
	$(FC) $(FFLAGS) loopsg.f -o loopsg

types : types.f
	$(FC) $(FFLAGS) types.f -o types

func : func.f
	$(FC) $(FFLAGS) func.f -o func

statf : statf.f
	$(FC) $(FFLAGS) statf.f -o statf

coms : coms.f
	$(FC) $(FFLAGS) coms.f -o coms

subs : subs.f
	$(FC) $(FFLAGS) subs.f -o subs

fileio : fileio.f
	$(FC) $(FFLAGS) fileio.f -o fileio

arith : arith.f
	$(FC) $(FFLAGS) arith.f -o arith

array : array.f
	$(FC) $(FFLAGS) array.f -o array

do : do.f
	$(FC) $(FFLAGS) do.f -o do

goto : goto.f
	$(FC) $(FFLAGS) goto.f -o goto

hello : hello.f
	$(FC) $(FFLAGS) hello.f -o hello

ifth : ifth.f
	$(FC) $(FFLAGS) ifth.f -o ifth

io : io.f
	$(FC) $(FFLAGS) io.f -o io

datas : datas.f
	$(FC) $(FFLAGS) datas.f -o datas

param : param.f
	$(FC) $(FFLAGS) param.f -o param

savey : savey.f
	$(FC) $(FFLAGS) savey.f -o savey

loopml : loopml.f
	$(FC) $(FFLAGS) loopml.f -o loopml

implml : implml.f
	$(FC) $(FFLAGS) implml.f -o implml

impl : impl.f
	$(FC) $(FFLAGS) impl.f -o impl

fmtio : fmtio.f
	$(FC) $(FFLAGS) fmtio.f -o fmtio

chars : chars.f
	$(FC) $(FFLAGS) chars.f -o chars

intfil : intfil.f
	$(FC) $(FFLAGS) intfil.f -o intfil
