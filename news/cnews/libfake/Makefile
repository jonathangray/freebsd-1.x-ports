INCLUDE = ../include
COPTS = -O
CFLAGS = $(COPTS) -I$(INCLUDE)
# workaround for System V make bug
SHELL = /bin/sh

ALL = fsync.o getopt.o memchr.o memcmp.o memcpy.o \
memset.o mkdir.o putenv.o strchr.o strcspn.o strpbrk.o strrchr.o \
strspn.o strtok.o symlink.o dbmclose.o dbz.o strerror.o

# beware -- build knows about NEEDED
NEEDED =  strerror.o

u:	$(NEEDED)
	ar ruv ../libcnews.a $(NEEDED)

all:	$(NEEDED)

try:	$(ALL)

clean:
	rm -f *.o
