SRC=fgets.c fputs.c rdwr.c
OBJ=fgets.o fputs.o rdwr.o
# PTR_TYPE is the type of _ptr in stdio.h, if not "char *"
DEFINES = '-DPTR_TYPE=unsigned char *' -I../include
COPTS = -O
CFLAGS = $(DEFINES) $(COPTS)
LINTFLAGS = $(DEFINES) -ha
LIBS = ../libcnews.a
# workaround for System V make bug
SHELL = /bin/sh

u:	$(OBJ)
	ar ruv ../libcnews.a $(OBJ)

all:	$(OBJ) stdiock.stock stdiock.fast runtrials

trials:	all
	chmod +x runtrials
	: run first with stdout directed to a plain file
	./runtrials >junk
	: and again with stdout directed to a tty, we hope.
	: do not panic.  you can ignore the output of runtrials.
	: only its exit status really matters.
	./runtrials
	rm stdiock.stock stdiock.fast junk

lint:
	lint $(LINTFLAGS) $(SRC) stdiock.c
lintport:
	lint -p $(LINTFLAGS) $(SRC) stdiock.c

stdiock.stock: stdiock.o $(LIBS)
	: search -lc before libcnews.a to get stock stdio
	: -n is an attempt to force static loading on shared-library systems
	$(CC) $(CFLAGS) $(LDFLAGS) -n stdiock.o $(PRE) -lc $(LIBS) $(POST) -o $@
stdiock.fast: stdiock.o $(OBJ) $(LIBS)
	: load *.o by hand to get tuned stdio
	$(CC) $(CFLAGS) $(LDFLAGS) stdiock.o $(OBJ) $(PRE) $(LIBS) $(POST) -o $@

clean:
	rm -f *.o stdiock stdiock.fast stdiock.stock junk *mon.out
