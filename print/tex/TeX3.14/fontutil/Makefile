# This file makes some font utilities (that are not part of MFware)
# from their respective sources.

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


default:	pktogf

all:		pktogf pktype vftovp vptovf


# Routines used everywhere.
# 
commondefines=../common/common.defines
commonh=../common/extra.h ../common/common.h
commono=../common/alloca.o ../common/extra.o ../common/main.o

# PKtoGF:
# 
pktogf:		pktogf.o $(commono)
		$(CC) -o pktogf $(LDFLAGS) pktogf.o $(commono) $(LOADLIBES)
pktogf.o:	pktogf.c $(commonh)
pktogf.c:	pktogf.p $(commondefines)
pktogf.p:	pktogf.web pktogf.ch

# PKtype:
# 
pktype:		pktype.o $(commono)
		$(CC) -o pktype $(LDFLAGS) pktype.o $(commono) $(LOADLIBES)
pktype.o:	pktype.c $(commonh)
pktype.c:	pktype.p $(commondefines)
pktype.p:	pktype.web pktype.ch

# VFtoVP:
# 
vftovp:		vftovp.o $(commono)
		$(CC) -o vftovp $(LDFLAGS) vftovp.o $(commono) $(LOADLIBES)
vftovp.o:	vftovp.c $(commonh)
		$(CC) $(CFLAGS) -c -Dindex=index_var -Dlink=link_array vftovp.c
vftovp.c:	vftovp.p $(commondefines)
vftovp.p:	vftovp.web vftovp.ch

# VPtoVF:
# 
vptovf:		vptovf.o $(commono)
		$(CC) -o vptovf $(LDFLAGS) vptovf.o $(commono) $(LOADLIBES)
vptovf.o:	vptovf.c $(commonh)
		$(CC) $(CFLAGS) -c -Dindex=index_arr -Dlink=link_array vptovf.c
vptovf.c:	vptovf.p $(commondefines)
vptovf.p:	vptovf.web vptovf.ch

clean:
		rm -f *.pool *.o pktogf pktype vftovp vptovf

veryclean:	clean
		rm -f *.p
		rm -f pktogf.c pktype.c vftovp.c vptovf.c
		rm -f pktogf.h pktype.h vftovp.h vptovf.h
		rm -f \#*\# *~ *.bak *.ckp core
