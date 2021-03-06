# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's readline directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ..

srcdir = .
VPATH = .

include $(TOPDIR)/Makeconf

INCFLAGS = -I. -I$(srcdir)/..

SOURCES = $(wildcard $(srcdir)/*.c)

INCLUDES = $(wildcard $(srcdir)/*.h)

DISTFILES = COPYING Makefile.in ChangeLog $(SOURCES) $(INCLUDES)

SUBDIRS = doc examples

# This is why you need GNU make.  Doing this without functions is too
# ugly to think about.

READLINE_CSRC = $(wildcard $(srcdir)/*.c)
READLINE_BAS1 = $(notdir $(READLINE_CSRC))

# Don't compile emacs_keymap.c and vi_keymap.c since they are included
# by keymaps.c.

READLINE_BAS2 = $(subst emacs_keymap.c, , $(READLINE_BAS1))
READLINE_BASE = $(subst vi_keymap.c, , $(READLINE_BAS2))

READLINE_OBJ = $(patsubst %.c, %.o, $(READLINE_BASE))

LIBREADLINE_DEPEND := $(patsubst %, ../libreadline.a(%), $(READLINE_OBJ))

# Define TYPES as -DVOID_SIGHANDLER if your operating system uses
# a return type of "void" for signal handlers.
# TYPES = -DVOID_SIGHANDLER

# Define USG as -DUSG if you are using a System V operating system.
#USG = -DUSG

# HP-UX compilation requires the BSD library.
#LOCAL_LIBS = -lBSD

# Xenix compilation requires -ldir -lx
#LOCAL_LIBS = -ldir -lx

# Comment out "-DVI_MODE" if you don't think that anyone will ever desire
# the vi line editing mode and features.  Make sure to comment out the
# definition of VI if you remove the -DVI_MODE.
READLINE_DEFINES = $(TYPES) -DVI_MODE

# Override Makeconf's definition of ALL_CFLAGS.
ALL_CFLAGS = $(INCFLAGS) $(DEFS) $(READLINE_DEFINES) $(CFLAGS)

all: ../libreadline.a
.PHONY: all

../libreadline.a: $(LIBREADLINE_DEPEND)
	$(RANLIB) ../libreadline.a

check: all
.PHONY: check

install: all
	if test -d $(libdir) ; then true ; else $(TOPDIR)/mkpath $(libdir) ; fi
	rm -f $(libdir)/libreadline.a
	$(INSTALL_DATA) ../libreadline.a $(libdir)/libreadline.a
	$(RANLIB) $(libdir)/libreadline.a
.PHONY: install

uninstall:
	rm -f $(libdir)/libreadline.a
.PHONY: uninstall

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

clean:
	rm -f *.a *.o
.PHONY: clean

mostlyclean: clean
.PHONY: mostlyclean

distclean: clean
	rm -f Makefile ../libreadline.a
.PHONY: distclean

realclean: distclean
	rm -f tags TAGS
.PHONY: realclean

local-dist:
	for dir in $(SUBDIRS); do mkdir ../`cat ../.fname`/readline/$$dir; cd $$dir; $(MAKE) $@; cd ..; done
	ln $(DISTFILES) ../`cat ../.fname`/readline
.PHONY: local-dist

dist:
	for dir in $(SUBDIRS); do mkdir ../`cat ../.fname`/readline/$$dir; cd $$dir; $(MAKE) $@; cd ..; done
	ln $(DISTFILES) ../`cat ../.fname`/readline
.PHONY: dist

readline.o:	readline.c readline.h chardefs.h  keymaps.h history.h
history.o:	history.c history.h
vi_mode.o:	vi_mode.c
funmap.o:	funmap.c readline.h
keymaps.o:	keymaps.c emacs_keymap.c vi_keymap.c keymaps.h chardefs.h
