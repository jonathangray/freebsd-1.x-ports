# This file makes BibTeX.

SITEDIR=..
CC=gcc2
OPT=-g
CFLAGS=$(OPT) -I$(SITEDIR) -DBibTeX
LDFLAGS=
LOADLIBES=-lcposix
SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .c .p .ch
.p.c:
		$(SHELL) ./convert $*.p $*.c
.ch.p:
		../web/tangle $*.web $*.ch
.c.o:
		$(CC) $(CFLAGS) -c $*.c

# Routines used everywhere.
# 
commondefines=../common/common.defines ../common/texmf.defines
commonh=../common/extra.h ../common/common.h
commono=../common/alloca.o ../common/extra.o ../common/main.o

# Routines also used in TeX and Metafont (only).
# 
extrac=../common/fileio.c
extrah=../common/fileio.h
extrao=fileio.o


default:	all

all:		bibtex


bibtex:		bibtex.o $(commono) $(extrao)
		$(CC) -o bibtex $(LDFLAGS) bibtex.o $(commono) $(extrao) \
                  $(LOADLIBES)
bibtex.o:	bibtex.c $(commonh) $(extrah)
		$(CC) $(CFLAGS) -Dbswap=bswapp -c bibtex.c
bibtex.c:	bibtex.p $(commondefines) convert
bibtex.p:	bibtex.web bibtex.ch

$(extrao):	$(extrac) $(extrah)
		$(CC) $(CFLAGS) -I. -c $(extrac)


clean:
		rm -f bibtex.pool *.o bibtex

veryclean:	clean
		rm -f bibtex.p bibtex.c bibtex.h coerce.h
		rm -f \#*\# *~ *.bak *.ckp core
