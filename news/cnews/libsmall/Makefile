# libsmall makefile
INCLUDE=../include
DEFINES=-I$(INCLUDE) -I../relay
COPTS= -O # -pg -p
CFLAGS= $(COPTS) $(DEFINES)
LINTFLAGS=-hau $(DEFINES)
LIB=libcnews.a
# RANLIB is ranlib on non-USG systems, echo on USG systems
RANLIB=ranlib
#RANLIB=:
SRCS=active.slow.c sys.slow.c
OBJS=active.slow.o sys.slow.o
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
