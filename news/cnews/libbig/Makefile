# libbig makefile
INCLUDE=../include
DEFINES=-I$(INCLUDE) -I../relay
COPTS= -O # -g -p -pg
CFLAGS= $(COPTS) $(DEFINES)
LINTFLAGS=-hau $(DEFINES)
LIB=libbig.a
# RANLIB is ranlib on non-USG systems, echo on USG systems
RANLIB=ranlib
#RANLIB=:
SRCS=active.fast.c sys.fast.c
OBJS=active.fast.o sys.fast.o
# workaround for System V make bug
SHELL = /bin/sh

u:	$(OBJS)
	ar ruv ../libcnews.a $(OBJS)

all:	$(OBJS)

$(LIB): $(SRCS)
	$(CC) $(CFLAGS) -c $?
	ar rv $@ *.o
	rm *.o
	$(RANLIB) $@

lint:
	lint $(LINTFLAGS) $(SRCS)

clean:
	rm -f *.o
