# Generated automatically from Makefile.in by configure.
### Start of system configuration section ###

srcdir = .
VPATH = .

CC = gcc

DEFS = -DHAVE_CONFIG_H

CFLAGS = -g

### End of system configuration section ###

SHELL = /bin/sh

SRCS = cmp.c ls.c print.c util.c getopt.c
OBJS = cmp.o ls.o print.o util.o getopt.o

all: $(bsdlib)

INCLUDES = -I$(configdir) -I$(srcdir) -I$(srcdir)/$(commondir) -I$(srcdir)/$(clientdir)

.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(DEFS) $(INCLUDES) $<

install: all

uninstall install-man:
	@echo "nothing to do in `$(bsddir)' for `$@'"

$(bsdlib): $(OBJS)
	$(AR) rcu $@ $(OBJS)
	-$(RANLIB) $@

clean:
	-rm -f core *.o *.a

mostlyclean: clean

distclean: clean
	-rm -f Makefile

realclean: distclean

dist:

depend:
	$(commondir)/makedepend Makefile.in $(CFLAGS) $(CPPFLAGS) $(DEFS) $(INCLUDES) $(SRCS)

#############################################################################

COMMON = $(clientdir)/client.h $(commondir)/common.h

cmp.o:		cmp.c ls.h $(COMMON)
ls.o:		ls.c ls.h $(COMMON)
print.o:	print.c ls.h $(COMMON)
util.o:		util.c $(COMMON)
getopt.o:	getopt.c $(COMMON)
