# Generated automatically from Makefile.in by configure.
### Start of system configuration section ###

srcdir	= .
VPATH	= .

CC	= gcc

DEFS = -DHAVE_CONFIG_H

CFLAGS = -g

### End of system configuration section ###

SHELL = /bin/sh

SRCS = lbuffer.c lburst.c ldatestamp.c ldebug.c ldelay.c lecho.c lhash.c lif.c llcd.c lmacro.c lopen.c lpager.c lport.c lprompt.c lpwd.c lquit.c lreadme.c lrehash.c lshell.c lsince.c lskipto.c lsource.c ltimeout.c ltrace.c
OBJS = lbuffer.o lburst.o ldatestamp.o ldebug.o ldelay.o lecho.o lhash.o lif.o llcd.o lmacro.o lopen.o lpager.o lport.o lprompt.o lpwd.o lquit.o lreadme.o lrehash.o lshell.o lsince.o lskipto.o lsource.o ltimeout.o ltrace.o
PROGS = fsp

all: $(locallib)

INCLUDES = -I$(configdir) -I$(srcdir) -I$(srcdir)/$(commondir) \
	   -I$(srcdir)/$(bsddir) -I$(srcdir)/$(clientdir)

.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(DEFS) $(INCLUDES) \
		-Dmain=$*_main $<

$(locallib): util.o $(OBJS)
	$(AR) rcu $@ util.o $(OBJS)
	-$(RANLIB) $@

install: all

uninstall install-man:
	@echo "nothing to do in `$(localdir)' for `$@'"

clean:
	-rm -f *.o *.a

mostlyclean: clean

distclean: clean
	rm -f Makefile

realclean: distclean

dist:

depend:
	$(commondir)/makedepend Makefile.in $(CFLAGS) $(CPPFLAGS) $(INCLUDES) util.c $(SRCS)

util.o: util.c $(clientdir)/client.h $(clientdir)/main.h util.h
