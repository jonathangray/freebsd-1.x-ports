## -*- text -*- ####################################################
#								   #
# Makefile for readline and history libraries.			   #
#								   #
####################################################################

# Here is a rule for making .o files from .c files that doesn't force
# the type of the machine (like -sun3) into the flags.
.c.o:
	$(CC) -c $(CFLAGS) $(LOCAL_INCLUDES) $(CPPFLAGS) $*.c

# Destination installation directory.  The libraries are copied to DESTDIR
# when you do a `make install', and the header files to INCDIR/readline/*.h.
DESTDIR = /usr/gnu/lib
INCDIR = /usr/gnu/include

# Define TYPES as -DVOID_SIGHANDLER if your operating system uses
# a return type of "void" for signal handlers.
TYPES = -DVOID_SIGHANDLER

# Define USG as -DUSG if you are using a System V operating system.
#USG = -DUSG

# HP-UX compilation requires the BSD library.
#LOCAL_LIBS = -lBSD

# Xenix compilation requires -ldir -lx
#LOCAL_LIBS = -ldir -lx

# Comment out "-DVI_MODE" if you don't think that anyone will ever desire
# the vi line editing mode and features.
READLINE_DEFINES = $(TYPES) #-DVI_MODE

DEBUG_FLAGS = -O2
LDFLAGS = $(DEBUG_FLAGS) 
CFLAGS = $(DEBUG_FLAGS) $(USG) -I.

# A good alternative is gcc -traditional.
#CC = gcc -traditional
CC = cc
RANLIB = /usr/bin/ranlib
AR = ar
RM = rm
CP = cp

LOCAL_INCLUDES = -I../

CSOURCES = readline.c history.c funmap.c keymaps.c vi_mode.c \
	   emacs_keymap.c vi_keymap.c

HSOURCES = readline.h chardefs.h history.h keymaps.h
SOURCES  = $(CSOURCES) $(HSOURCES)

DOCSOURCE = doc/rlman.texinfo doc/rltech.texinfo doc/rluser.texinfo \
	    doc/hist.texinfo doc/hsuser.texinfo doc/hstech.texinfo
DOCOBJECT = doc/readline.dvi doc/history.dvi
DOCSUPPORT = doc/Makefile doc/texinfo.tex
DOCUMENTATION = $(DOCSOURCE) $(DOCOBJECT) $(DOCSUPPORT)

SUPPORT = COPYING Makefile $(DOCUMENTATION) ChangeLog examples

THINGS_TO_TAR = $(SOURCES) $(SUPPORT)

##########################################################################

all: libreadline.a

libreadline.a:	readline.o history.o funmap.o keymaps.o
	$(RM) -f libreadline.a
	$(AR) clq libreadline.a readline.o history.o funmap.o keymaps.o
	-if [ -f $(RANLIB) ]; then $(RANLIB) libreadline.a; fi

readline.o:	readline.c readline.h chardefs.h  keymaps.h history.h vi_mode.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(READLINE_DEFINES) \
	  $(LOCAL_INCLUDES) $*.c

history.o:	history.c history.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(READLINE_DEFINES) \
	  $(LOCAL_INCLUDES) $*.c

funmap.o:	funmap.c readline.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(READLINE_DEFINES) \
	  $(LOCAL_INCLUDES) $*.c

keymaps.o:	keymaps.c emacs_keymap.c vi_keymap.c keymaps.h chardefs.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(READLINE_DEFINES) \
	  $(LOCAL_INCLUDES) $*.c

libtest:	libreadline.a libtest.c
	$(CC) -o libtest $(CFLAGS) $(CPPFLAGS) \
	 -L. libtest.c -lreadline -ltermcap

readline: readline.c history.o keymaps.o funmap.o readline.h chardefs.h
	$(CC) $(CFLAGS) $(CPPFLAGS) $(READLINE_DEFINES) \
	  $(LOCAL_INCLUDES) -DTEST -o readline readline.c funmap.o \
	  keymaps.o history.o -L. -ltermcap

documentation: force
	(cd doc; make)
force:

readline.tar:	$(THINGS_TO_TAR)
	tar -cf readline.tar $(THINGS_TO_TAR)

readline.tar.Z:	readline.tar
	compress -f readline.tar

install:	$(DESTDIR)/libreadline.a includes

# The rule for 'includes' is written funny so that the if statement
# always returns TRUE unless there really was an error installing the
# include files.
includes:
	if [ -r $(INCDIR)/readline ]; then \
	  :; \
	else \
	  mkdir $(INCDIR)/readline && chmod a+r $(INCDIR)/readline; \
	fi
	$(CP) readline.h keymaps.h chardefs.h $(INCDIR)/readline/
clean:
	rm -f *.o *.a
	(cd doc; make clean)

$(DESTDIR)/libreadline.a: libreadline.a
	-mv $(DESTDIR)/libreadline.a $(DESTDIR)/libreadline.old
	cp libreadline.a $(DESTDIR)/libreadline.a
	if [ -f $(RANLIB) ]; then $(RANLIB) -t $(DESTDIR)/libreadline.a; fi

package: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST readline_bin
