# This file makes routines that are used everywhere.

SITEDIR=..
CC=gcc
OPT=-g
CFLAGS=$(OPT) -I$(SITEDIR)
LDFLAGS=
LOADLIBES=-lcposix
SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .c
.c.o:
		$(CC) $(CFLAGS) -c $*.c

default: all

triptrap: all
all: alloca.o endian.h extra.o getopt.o getopt1.o main.o

alloca.o: alloca.c

endian.h: endian
	./endian > endian.h

endian: endian.c
	$(CC) -o endian $(LDFLAGS) $(CFLAGS) endian.c $(LOADLIBES)
	
extra.o: extra.c extra.h $(SITEDIR)/site.h

getopt1.o: getopt1.c getopt.h

main.o: main.c extra.h $(SITEDIR)/site.h

# We don't make `texmf.o', since TeX and Metafont need to use
# different names, hence they need different .o files.


clean:
	rm -f *.o endian endian.h

veryclean: clean
