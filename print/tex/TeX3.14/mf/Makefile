# This file makes Metafont in its various incarnations.

SITEDIR=..
CC=gcc
OPT=-g -O
CFLAGS=$(OPT) -I$(SITEDIR) -I/usr/X386/include
LDFLAGS=-L/usr/X386/lib
LOADLIBES=-lcposix
SHELL=/bin/sh
LN=ln

# The bases we know how to make.
bases=plain.base cmmf.base

# Name of the file that defines the local devices.
localmodes=modes.mf

# System libraries for window support.
wlibs=

makeargs=SITEDIR="$(SITEDIR)" CC="$(CC)" OPT="$(OPT)" \
         LDFLAGS="$(LDFLAGS)" LOADLIBES="$(LOADLIBES)" wlibs="$(wlibs)"

.SUFFIXES:
.SUFFIXES: .o .c
.c.o:
		$(CC) $(CFLAGS) -c $*.c


# Routines used everywhere.
# 
commondefines=../common/common.defines ../common/texmf.defines
commonh=../common/extra.h ../common/common.h
commono=../common/alloca.o ../common/extra.o

# Routines used in both TeX and Metafont.
# 
extrac=../common/texmf.c
extrah=../common/texmf.h ../common/endian.h
extrao=texmf.o

# Routines used in TeX, Metafont, and BibTeX.
# 
fileioc=../common/fileio.c
fileioh=../common/fileio.h
fileioo=fileio.o

windowlib=MFwindow/window.a
libs=$(windowlib) $(wlibs) 

# We don't add `$(commono)' here, since it doesn't depend on anything in
# this directory.
# 
objs=$(fileioo) mf0.o mf1.o mf2.o mf3.o mf4.o mf5.o mf6.o mf7.o mf8.o mf9.o
iobjs=iextra.o inimf.o $(objs)
vobjs=mf-extra.o imf.o $(objs)


default:	all

all:		inimf virmf
triptrap:	trapmf


# It is just a waste of disk space to have window support in inimf.
# 
inimf:		stamp-convert $(iobjs) $(commono)
		$(CC) -o inimf $(LDFLAGS) $(iobjs) $(commono) $(LOADLIBES)

virmf:		stamp-convert $(vobjs) $(windowlib) $(commono)
		$(CC) -o virmf $(LDFLAGS) $(vobjs) $(libs) $(commono) \
                  $(LOADLIBES)

run-trap:
		rm -f trap.mf
		-$(LN) MFtrap/trap.mf .
		-$(SHELL) -c '. ./trapenv; \
                  ./trapmf < MFtrap/trap1.in > /dev/null 2>&1'
		-diff MFtrap/trapin.log trap.log
		-$(SHELL) -c '. ./trapenv; \
                  ./trapmf < MFtrap/trap2.in > trap.fot'
		-diff MFtrap/trap.log trap.log
		-diff MFtrap/trap.fot trap.fot
		../texware/tftopl ./trap.tfm trap.pl
		-diff MFtrap/trap.pl trap.pl
		$(SHELL) -c '. ./trapenv; \
		  ../mfware/gftype -m -i ./trap.72270gf > trap.typ'
		-diff MFtrap/trap.typ trap.typ


# Make base files automatically.
# 
bases:	$(bases)

mf.base:
	./inimf 'plain; input $(localmodes); dump'
	mv plain.base mf.base
	mv plain.log mf.log

cmmf.base: mf.base
	./inimf \&./mf cmbase dump
	mv cmbase.base cmmf.base
	mv cmbase.log cmmf.log


# The `ini_to_trap' script changes mfd.h, instead of the change file, so
# retangling is not necessary.  After changing constants, making a
# trapmf means making an inimf.
# 
trapmf:		stamp-trap $(iobjs) $(commono)
		make $(makeargs) inimf
		mv inimf trapmf

stamp-trap:	stamp-convert
		rm -f *.o
		$(SHELL) ./ini_to_trap mfd.h
		touch stamp-trap

# tangle produces mf.p and mf.pool.
# 
mf.p:		mf.web cmf.ch
		../web/tangle mf.web cmf.ch

# The convert script produces mf[0-9].c, imf.c, coerce.h, and mfd.h.
# The (hand-coded) memory.h is included in the mf?.c files; it isn't
# really necessary to `convert' again if it changes, only to recompile
# those files, but describing that relationship is ugly to do.
# (Furthermore, memory.h is stable.)
# 
stamp-convert:	mf.p memory.h fixcoerce.h $(commondefines)
		$(SHELL) ./convert
		touch stamp-convert

# The (hand-coded) file $(extrac) and the (generated) file imf.c have
# #ifdefs for INIMF, so we have to compile them differently.
# 
iextra.o:	$(extrac) mfd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -I. -DINIMF -DINI -c $(extrac)
		mv $(extrao) iextra.o

inimf.o:	imf.c mfd.h $(extrah) $(commonh)
		$(CC) $(CFLAGS) -DINIMF -c imf.c && mv imf.o inimf.o

$(objs):	stamp-convert mfd.h

mf-extra.o:	$(extrac) mfd.h
		rm -f mf-extra.c
		$(LN) $(extrac) mf-extra.c
		$(CC) $(CFLAGS) -I. -c mf-extra.c

$(fileioo):	$(fileioc) $(fileioh)
		$(CC) $(CFLAGS) -I. -c $(fileioc)

mf0.o:		mf0.c
mf1.o:		mf1.c
mf2.o:		mf2.c
mf3.o:		mf3.c
mf4.o:		mf4.c
mf5.o:		mf5.c
mf6.o:		mf6.c
mf7.o:		mf7.c
mf8.o:		mf8.c
mf9.o:		mf9.c

$(windowlib):
		cd MFwindow; make $(makeargs)

clean:
		rm -f stamp-* *.o trapmf inimf virmf
		rm -f mfd.h coerce.h *.log *.base
		rm -f trap.* mf-extra.c
		cd MFwindow; make clean

veryclean:	clean
		rm -f mf?.c imf.c mf.p mf.pool mfput.*
		rm -f \#*\# *~ *.bak *.ckp core
		cd MFwindow; make veryclean
