# This file makes Tangle and Weave from their WEB sources, or, in case
# of tangle, possibly from a bootstrap C file.

SITEDIR=..
CC=gcc
OPT=-g
CFLAGS=$(OPT) -Dlink=link_array -I$(SITEDIR)
LDFLAGS=
LOADLIBES=-lcposix
SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .c .p .ch
.p.c:
		$(SHELL) ../common/convert $*.p $*.c
.ch.p:
		./tangle $*.web $*.ch
.c.o:
		$(CC) $(CFLAGS) -c $*.c


default:	tangle

all:		tangle weave

# Routines used everywhere.
# 
commondefines=../common/common.defines
commonh=../common/extra.h ../common/common.h
commonc=../common/extra.c ../common/extra.o
commono=../common/alloca.o ../common/extra.o ../common/main.o

# Tangle:
# 
tangle:		tangle.o $(commono)
		$(CC) -o tangle $(LDFLAGS) tangle.o $(commono) $(LOADLIBES)
tangle.o: 	tangle.c $(commonh)
tangle.c: 	tangle.p $(commondefines)

# Without the nested $(SHELL) -c's, the AIX sh does not execute
# the loop at all.
tangle.p:	tangle.web tangle.ch
		$(SHELL) -c "until (PATH=.:$${PATH}			\
                  $(SHELL) -c tangle\ tangle.web\ tangle.ch )		\
                  do							\
		    $(CC) $(LDFLAGS) -o tangle $(CFLAGS) tangleboot.c	\
                      $(commono) $(LOADLIBES);				\
		  done"

# Weave:
# 
weave: 		weave.o $(commono)
		$(CC) -o weave $(LDFLAGS) weave.o $(commono) $(LOADLIBES)
weave.o: 	weave.c $(commonh)
weave.c:	weave.p $(commondefines)
weave.p: 	weave.web weave.ch

clean:
		rm -f *.o *.pool weave tangle coerce.h

# Do not remove tangle.h, since it's needed for bootstrapping.
# 
veryclean:	clean
		rm -f weave.c tangle.c weave.h weave.p tangle.p
		rm -f \#*\# *~ *.bak *.ckp core
