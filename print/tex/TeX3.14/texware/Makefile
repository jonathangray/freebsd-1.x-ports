# This file makes DVItype, PatGen, PLtoTF, TFtoPL, and POOLtype.

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


default:	pltotf tftopl


all:		triptrap patgen pooltype
triptrap:	dvitype pltotf tftopl


# Routines used everywhere.
# 
commondefines=../common/common.defines
commonh=../common/extra.h ../common/common.h
commono=../common/alloca.o ../common/extra.o ../common/main.o \
        ../common/getopt.o ../common/getopt1.o

# DVItype:
# 
dvitype:	dvitype.o $(commono)
		$(CC) -o dvitype $(LDFLAGS) dvitype.o $(commono) $(LOADLIBES)
dvitype.o:	dvitype.c $(commonh)
dvitype.c:	dvitype.p $(commondefines)
dvitype.p:	dvitype.web dvitype.ch

# PatGen:
# 
patgen:		patgen.o $(commono)
		$(CC) -o patgen $(LDFLAGS) patgen.o $(commono) $(LOADLIBES)
patgen.o:	patgen.c $(commonh)
patgen.c:	patgen.p $(commondefines)
patgen.p:	patgen.web patgen.ch

# PLtoTF:
# 
pltotf:		pltotf.o $(commono)
		$(CC) -o pltotf $(LDFLAGS) pltotf.o $(commono) $(LOADLIBES)
pltotf.o:	pltotf.c $(commonh)
		$(CC) $(CFLAGS) -c -Dindex=index_arr -Dlink=link_array pltotf.c
pltotf.c:	pltotf.p $(commondefines)
pltotf.p:	pltotf.web pltotf.ch

# POOLtype:
# 
pooltype:	pooltype.o $(commono)
		$(CC) -o pooltype $(LDFLAGS) pooltype.o $(commono) $(LOADLIBES)
pooltype.o:	pooltype.c $(commonh)
pooltype.c:	pooltype.p $(commondefines)
pooltype.p:	pooltype.web pooltype.ch

# TFtoPL:
# 
tftopl:		tftopl.o $(commono)
		$(CC) -o tftopl $(LDFLAGS) tftopl.o $(commono) $(LOADLIBES)
tftopl.o:	tftopl.c $(commonh)
		$(CC) $(CFLAGS) -c -Dindex=index_var tftopl.c
tftopl.c:	tftopl.p $(commondefines)
tftopl.p:	tftopl.web tftopl.ch


clean:
		rm -f *.pool *.o dvitype patgen pltotf pooltype tftopl

veryclean:	clean
		rm -f *.p
		rm -f dvitype.c patgen.c pltotf.c pooltype.c tftopl.c
		rm -f dvitype.h patgen.h pltotf.h pooltype.h tftopl.h
		rm -f \#*\# *~ *.bak *.ckp core
