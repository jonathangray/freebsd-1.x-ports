# Generated automatically from Makefile.in by configure.
### Start of system configuration section ###

srcdir	= .
VPATH	= .

CC	= cc

DEFS = -DHAVE_CONFIG_H

CFLAGS = -g

### End of system configuration section ###

SHELL = /bin/sh

SRCS = rcat.c rcd.c rdu.c rget.c rgrab.c rls.c rmkdir.c rpro.c rput.c rrm.c rrmdir.c rtar.c rtouch.c rver.c
OBJS = rcat.o rcd.o rdu.o rget.o rgrab.o rls.o rmkdir.o rpro.o rput.o rrm.o rrmdir.o rtar.o rtouch.o rver.o

all: $(remotelib)

INCLUDES = -I$(configdir) -I$(srcdir) -I$(srcdir)/$(commondir) \
	   -I$(srcdir)/$(bsddir) -I$(srcdir)/$(clientdir)

.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(DEFS) $(INCLUDES) \
		-Dmain=$*_main $<

$(remotelib): $(OBJS)
	$(AR) rcu $@ $(OBJS)
	-$(RANLIB) $@

install: all

uninstall install-man:
	@echo "nothing to do in `$(remotedir)' for `$@'"

clean:
	-rm -f *.o *.a

mostlyclean: clean

distclean: clean
	rm -f Makefile

realclean: distclean

dist:

depend:
	$(commondir)/makedepend Makefile.in $(CFLAGS) $(CPPFLAGS) $(INCLUDES) $(SRCS)
