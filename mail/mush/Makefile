# makefile.bsd	(c) copyright 1991	(Dan Heller)
# SunOS users should add -DSUN_3_5, -DSUN_4_0, or -DSUN_4_1 to CFLAGS.
#
HDRS= mush.h config.h-dist strings.h bindings.h options.h version.h glob.h pop.h

SRCS= main.c init.c misc.c mail.c hdrs.c execute.c commands.c print.c dates.c \
      signals.c setopts.c msgs.c pick.c sort.c expr.c folders.c \
      loop.c viewopts.c curses.c curs_io.c bind.c file.c strings.c \
      lock.c macros.c options.c addrs.c malloc.c glob.c command2.c \
      pop.c pmush.c xcreat.c
OBJS= main.o init.o misc.o mail.o hdrs.o execute.o commands.o print.o file.o \
      signals.o setopts.o msgs.o pick.o sort.o expr.o strings.o \
      folders.o dates.o loop.o viewopts.o curses.o curs_io.o bind.o \
      lock.o macros.o options.o addrs.o malloc.o glob.o command2.o \
      pop.o pmush.o xcreat.o

HELP_FILES= README README-7.0 README-7.1 README-7.2.0 README-7.2.2 \
	README-7.2.4 mush.1 cmd_help Mushrc Mailrc Gnurc \
	sample.mushrc advanced.mushrc digestify

MAKES= makefile.bsd makefile.xenix makefile.sys.v makefile.hpux makefile.sun

# See the README for changes needed to compile under Ultrix.
# In particular, you may need -DSIGRET=void and/or -ltermcap.
CFLAGS= -O -DCURSES -DBSD -DFreeBSD -DREGEX -fwritable-strings
LDFLAGS=
LINTFLAGS= -bxah -Dlint -DCURSES -DBSD
LIBS= -lcurses -ltermlib
OTHERLIBS=
# Use some variant of this one if you #define MMDF in config.h
#OTHERLIBS=/usr/src/mmdf/lib/libmmdf.a

all: mush

mush: $(OBJS)
	@echo loading...
	@cc $(LDFLAGS) $(OBJS) $(LIBS) $(OTHERLIBS) -o mush

$(OBJS): config.h mush.h
loop.o: version.h

tape:
	@tar cv $(MAKES) $(HDRS) $(SRCS) $(HELP_FILES)

tar:
	@tar fcv MUSH $(MAKES) $(HDRS) $(SRCS) $(HELP_FILES)

tarmail:
	tar fcv - $(MAKES) $(HDRS) $(SRCS) $(HELP_FILES) | \
	compress | btoa > mush.tarmail

lint:
	lint $(LINTFLAGS) $(SRCS)

clean:
	rm -f *.o core core.* mush

BINDIR= /usr/local/bin
LIBDIR= /usr/local/lib
MRCDIR= /usr/local/lib
MANDIR= /usr/local/man/man1
MANEXT= 1

install: mush
	mv mush $(BINDIR)
	strip $(BINDIR)/mush
	chmod 0755 $(BINDIR)/mush
	cp mush.1 $(MANDIR)/mush.$(MANEXT)
	chmod 0644 $(MANDIR)/mush.$(MANEXT)
	cp cmd_help $(LIBDIR)
	chmod 0644 $(LIBDIR)/cmd_help
	cp Mushrc $(MRCDIR)/Mushrc
	chmod 0644 $(MRCDIR)/Mushrc

package: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST mush_bin

