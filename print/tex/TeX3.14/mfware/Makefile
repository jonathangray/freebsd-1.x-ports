# This file makes GFtoDVI, GFtoPK, GFtype, MFT, PKtoGF, and PKtype.

SITEDIR=..
CC=gcc
OPT=-g
CFLAGS=$(OPT) -I$(SITEDIR)
LDFLAGS=
LOADLIBES=-lcposix
SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .c .p .ch
.p.c:
		$(SHELL) ../common/convert $*.p $*.c
.ch.p:
		../web/tangle $*.web $*.ch
.c.o:
		$(CC) $(CFLAGS) -c $*.c

# Routines used everywhere.
# 
commondefines=../common/common.defines
commonh=../common/extra.h ../common/common.h
commono=../common/alloca.o ../common/extra.o ../common/main.o \
        ../common/getopt.o ../common/getopt1.o


default:	all

all:		triptrap gftodvi gftopk mft
triptrap:	gftype


# GFtoDVI:
# 
gftodvi:	gftodvi.o $(commono)
		$(CC) -o gftodvi $(LDFLAGS) gftodvi.o $(commono) $(LOADLIBES)
gftodvi.o:	gftodvi.c $(commonh) memory.h
gftodvi.c:	gftodvi.p $(commondefines)
gftodvi.p:	gftodvi.web gftodvi.ch

# GFtoPK:
# 
gftopk:		gftopk.o $(commono)
		$(CC) -o gftopk $(LDFLAGS) gftopk.o $(commono) $(LOADLIBES)
gftopk.o:	gftopk.c $(commonh)
gftopk.c:	gftopk.p $(commondefines)
gftopk.p:	gftopk.web gftopk.ch

# GFtype:
# 
gftype:		gftype.o $(commono)
		$(CC) -o gftype $(LDFLAGS) gftype.o $(commono) $(LOADLIBES) 
gftype.o:	gftype.c $(commonh)
gftype.c:	gftype.p $(commondefines)
gftype.p:	gftype.web gftype.ch

# MFT:
# 
mft:		mft.o $(commono)
		$(CC) -o mft $(LDFLAGS) mft.o $(commono) $(LOADLIBES)
mft.o:		mft.c $(commonh)
		$(CC) $(CFLAGS) -c -Dlink=link_array mft.c
mft.c:		mft.p $(commondefines)
mft.p:		mft.web mft.ch


clean:
		rm -f *.pool *.o gftodvi gftype gftopk mft

veryclean:	clean
		rm -f *.p
		rm -f gftodvi.c gftype.c gftopk.c mft.c
		rm -f gftodvi.h gftype.h gftopk.h mft.h
		rm -f \#*\# *~ *.bak *.ckp core
