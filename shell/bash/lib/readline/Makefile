## -*- text -*- ####################################################
#								   #
# Makefile for the GNU Readline and History Libraries.		   #
#								   #
####################################################################

# This Makefile is hand made from a template file, found in
# ../template.  Each library must provide several Makefile
# targets: `all', `clean', `documentation', `install', and
# `what-tar'.  The `what-tar' target reports the names of the
# files that need to be included in a tarfile to build the full
# code and documentation for this library.

# Please note that the values for INCLUDES, CC, AR, RM, CP,
# RANLIB, and selfdir are passed in from ../Makefile, and do
# not need to be defined here.

# Here is a rule for making .o files from .c files that doesn't force
# the type of the machine (like -sun3) into the flags.
.c.o:
	$(CC) -c $(CFLAGS) $(INCLUDES) $(LOCAL_DEFINES) $(CPPFLAGS) $*.c

# LOCAL_DEFINES are flags that are specific to this library.
# Comment out "-DVI_MODE" if you don't think that anyone will ever desire
# the vi line editing mode and features.  Define -DUSG if you are using a
# System V operating system.
LOCAL_DEFINES = $(LOCAL_INCLUDES) -DVI_MODE #-DUSG

# Some systems need to define this (like older NeXT systems) in order
# to prevent namespace conflicts.
CFLAGS = # -DSTATIC_MALLOC

# For libraries which include headers from other libraries.
LOCAL_INCLUDES = -I. -I../

# The name of the main library target.
LIBRARY_NAME = libreadline.a

# The C code source files for this library.
CSOURCES = readline.c funmap.c keymaps.c vi_mode.c parens.c \
	   rltty.c complete.c bind.c isearch.c display.c signals.c \
	   emacs_keymap.c vi_keymap.c history.c tilde.c xmalloc.c

# The header files for this library.
HSOURCES = readline.h rldefs.h chardefs.h keymaps.h history.h \
	   posixstat.h tilde.h

OBJECTS = readline.o vi_mode.o funmap.o keymaps.o parens.o search.o \
	  rltty.o complete.o bind.o isearch.o display.o signals.o \
	  history.o tilde.o xmalloc.o

# The texinfo files which document this library.
DOCSOURCE = doc/rlman.texinfo doc/rltech.texinfo doc/rluser.texinfo
DOCOBJECT = doc/readline.dvi
DOCSUPPORT = doc/Makefile
DOCUMENTATION = $(DOCSOURCE) $(DOCOBJECT) $(DOCSUPPORT)

SUPPORT = Makefile ChangeLog $(DOCSUPPORT) examples/[-a-z.]*

SOURCES  = $(CSOURCES) $(HSOURCES) $(DOCSOURCE)

THINGS_TO_TAR = $(SOURCES) $(SUPPORT)

##########################################################################

all: libreadline.a libhistory.a

libreadline.a: $(OBJECTS)
	$(RM) -f $@
	$(AR) cq $@ $(OBJECTS)
	-if [ -f "$(RANLIB)" ]; then $(RANLIB) $@; fi

libhistory.a: history.o
	$(RM) -f $@
	$(AR) cq $@ history.o
	-if [ -f "$(RANLIB)" ]; then $(RANLIB) $@; fi

what-tar:
	@for file in $(THINGS_TO_TAR); do \
	  echo $(selfdir)$$file; \
	done

documentation: force
	(cd doc; make)
force:

# The rule for 'includes' is written funny so that the if statement
# always returns TRUE unless there really was an error installing the
# include files.
install:
	if [ -r $(incdir)/readline ]; then \
	  :; \
	else \
	  mkdir $(incdir)/readline && chmod a+r $(incdir)/readline; \
	fi
	$(CP) readline.h keymaps.h chardefs.h history.h $(incdir)/readline/
	-mv $(bindir)/libreadline.a $(bindir)/libreadline.old
	cp libreadline.a $(bindir)/libreadline.a
	if [ -f "$(RANLIB)" ]; then $(RANLIB) -t $(bindir)/libreadline.a; fi

clean:
	rm -f $(OBJECTS) *.a
	(cd doc; make clean)

readline: readline.h rldefs.h chardefs.h
readline: $(OBJECTS)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(READLINE_DEFINES) \
	  $(LOCAL_INCLUDES) -DTEST -o readline readline.c vi_mode.o funmap.o \
	  keymaps.o -ltermcap

readline.o: readline.c readline.h rldefs.h chardefs.h
readline.o: keymaps.h history.h
vi_mode.o:  vi_mode.c
funmap.o:   funmap.c readline.h
keymaps.o:  keymaps.c emacs_keymap.c vi_keymap.c keymaps.h chardefs.h
