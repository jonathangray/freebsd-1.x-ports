INCLUDE=../include
DEFINES=-I$(INCLUDE)
COPTS=-O # -g -p
CFLAGS=$(COPTS) $(DEFINES)
# workaround for System V make bug
SHELL = /bin/sh

OBJS = closeall.o efopen.o error.o fgetmfs.o getdate.o nfclose.o \
	fgetfln.o cfgetln.o dogets.o \
	stdfdopen.o warning.o emalloc.o split.o \
	getabsdate.o getindate.o datetok.o dateconv.o qmktime.o hash.o hdbm.o

u:	$(OBJS)
	ar ruv ../libcnews.a $(OBJS)

all:	$(OBJS)

clean:
	rm -f *.o *.a getdate.c y.*.h y.*.c
