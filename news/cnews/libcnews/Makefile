# libcnews makefile
INCLUDE=../include
DEFINES=-I$(INCLUDE)
COPTS= -O # -pg -p
CFLAGS= $(COPTS) $(DEFINES)
LINTFLAGS=-hau $(DEFINES)
LIB=libcnews.a
# RANLIB is ranlib on non-USG systems, echo on USG systems
RANLIB=ranlib
#RANLIB=:
SRCS = complain.c config.c fopenclex.c gethdr.c hostname.c lock.c ltoza.c \
	nemalloc.c ngmatch.c str3save.c string.c strlower.c \
	strsave.c time.c case.c mkinperm.c unprivileged.c \
	trbatcomm.c fnlockfile.c msgs.c
#	newstok.c
OBJS = complain.o config.o fopenclex.o gethdr.o hostname.o lock.o ltoza.o \
	nemalloc.o ngmatch.o str3save.o string.o strlower.o \
	strsave.o time.o case.o mkinperm.o unprivileged.o \
	trbatcomm.o fnlockfile.o msgs.o
#	newstok.o
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

# header dependencies for libcnews.a
config.o: $(INCLUDE)/news.h $(INCLUDE)/config.h
lock.o fnlockfile.o: $(INCLUDE)/news.h
mkinperm.o: $(INCLUDE)/news.h
ngmatch.o: $(INCLUDE)/news.h
time.o: $(INCLUDE)/news.h
# ltoza.o: $(INCLUDE)/stdlib.h	# trouble if stdlib.h is system one, not ours
