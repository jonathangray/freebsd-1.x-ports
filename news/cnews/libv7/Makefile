# C news libv7 makefile
INCLUDE = ../include
DEFINES=-I$(INCLUDE)
COPTS=-O # -p
CFLAGS=$(COPTS) $(DEFINES)
LINTFLAGS=-hau $(DEFINES)
# workaround for System V make bug
SHELL = /bin/sh

SRCS = clsexec.c fopenexcl.c gethostname.c getcwd.c
OBJS = clsexec.o fopenexcl.o gethostname.o getcwd.o

# RANLIB is ranlib on non-USG systems, echo on USG systems
RANLIB=ranlib

u:	$(OBJS)
	ar ruv ../libcnews.a $(OBJS)

all:	$(OBJS)

libv7.a: $(SRCS)
	$(CC) $(CFLAGS) -c $?
	ar ru $@ *.o
	rm *.o
	$(RANLIB) $@
lint:
	lint $(LINTFLAGS) $(SRCS)

clean:
	rm -f *.o
