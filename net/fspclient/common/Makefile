# Generated automatically from Makefile.in by configure.
### Start of system configuration section ###

srcdir = .
VPATH = .

CC = gcc

DEFS = -DHAVE_CONFIG_H

CFLAGS = -g

### End of system configuration section ###

SHELL = /bin/sh

SRCS = udp_io.c misc.c
OBJS = udp_io.o misc.o

all: $(commonlib)

INCLUDES = -I$(configdir) -I$(srcdir)

.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(DEFS) $(INCLUDES) $<

$(commonlib):	$(OBJS)
	$(AR) rcu $@ $(OBJS)
	-$(RANLIB) $@

install: all

uninstall install-man:
	@echo "nothing to do in `$(commondir)' for `$@'"

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

misc.o:		misc.c common.h
udp_io.o:	udp_io.c common.h
