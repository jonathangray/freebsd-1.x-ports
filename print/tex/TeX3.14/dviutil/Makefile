# This file makes DVIcopy.

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


default:        all

all:            dvicopy


# Routines used everywhere.
#
commondefines=../common/common.defines
commonh=../common/extra.h ../common/common.h
commono=../common/alloca.o ../common/extra.o ../common/main.o

# DVIcopy:
#
dvicopy:        dvicopy.o $(commono)
		$(CC) -o dvicopy $(LDFLAGS) dvicopy.o $(commono) $(LOADLIBES)
dvicopy.o:      dvicopy.c $(commonh)
dvicopy.c:      dvicopy.p $(commondefines)
dvicopy.p:      dvicopy.web dvicopy.ch

clean:
		rm -f *.pool *.o dvicopy

veryclean:      clean
		rm -f *.p
		rm -f dvicopy.c
		rm -f dvicopy.h
		rm -f \#*\# *~ *.bak *.ckp core
