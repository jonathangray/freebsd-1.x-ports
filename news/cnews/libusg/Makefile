# C news libusg makefile
INCLUDE = ../include
DEFINES=-I$(INCLUDE)
COPTS=-O # -p
CFLAGS=$(COPTS) $(DEFINES)
LINTFLAGS=-hau $(DEFINES)
# workaround for System V make bug
SHELL = /bin/sh

SRCS=clsexec.c fopenexcl.c ftime.c gethostname.c
OBJS=clsexec.o fopenexcl.o ftime.o gethostname.o

# RANLIB is ranlib on non-USG systems, echo on USG systems
RANLIB=echo

u:	$(OBJS)
	ar ruv ../libcnews.a $(OBJS)

all:	$(OBJS)

libusg.a: $(SRCS)
	$(CC) $(CFLAGS) -c $?
	ar ru $@ *.o
	rm *.o
	$(RANLIB) $@
lint:
	lint $(LINTFLAGS) $(SRCS)

clean:
	rm -f *.o
