# Generated automatically from Makefile.in by configure.
# ------------------------------------------------------------------------
#       Makefile for demonstation shell of BLT library
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
#       C Compiler options 
# ------------------------------------------------------------------------

DEFINES      = 
CFLAGS       = -O
CC 	     = cc

TOPDIR	     = /opt/cellar/deli/blt
BINDIR       = /opt/cellar/deli/blt/bin

# ------------------------------------------------------------------------
#       Include directives for Tcl, Tk, and X include files 
# ------------------------------------------------------------------------

TCL_INCLUDE   = -I/opt/cellar/tcl/tcl7.3/include
TK_INCLUDE    = -I/opt/cellar/tk/tk3.6/include
X_INCLUDE     = -I/usr/X11/include

# ------------------------------------------------------------------------
#       Libraries directives for Tcl, Tk, X11, and BLT
# ------------------------------------------------------------------------

X_LIB        = -L/usr/X11/lib -lX11
TK_LIB       = -L/opt/cellar/tk/tk3.6/lib -ltk
TCL_LIB      = -L/opt/cellar/tcl/tcl7.3/lib -ltcl
BLT_LIB      = ./src/libBLT.a
EXTRA_LIBS   =  -lnsl -lm #-R/opt/cellar/tk/tk3.6/lib:/opt/cellar/tcl/tcl7.3/lib:/usr/X11/lib


# ------------------------------------------------------------------------
#       Don't edit anything beyond this point
# ------------------------------------------------------------------------

demo 	     = blt_wish
INCLUDES     = -I. $(TK_INCLUDE) $(TCL_INCLUDE) $(X_INCLUDE)
CC_SWITCHES  = $(CFLAGS) $(DEFINES) $(INCLUDES)
LIBRARIES    = $(BLT_LIB) $(TK_LIB) $(TCL_LIB) $(X_LIB) $(EXTRA_LIBS)
INSTALL      = /bin/install -c
RANLIB 	     = ranlib
SHELL 	     = /bin/sh
RM 	     = rm -f
srcdir	     = .

all: $(demo)

$(demo): normallib
	@echo "linking BLT demo as \"$(demo)\"..."
	$(CC) $(CC_SWITCHES) $(srcdir)/tkAppInit.c -o $(demo) $(LIBRARIES)
	@echo "make finished on `date`"

normallib:
	@echo "making all in ./src"
	@(cd src; $(MAKE) CFLAGS="$(CFLAGS)" all)

sharedlib: 
	@echo "making shared library in ./src/shared"
	@(cd src/shared; $(MAKE) CFLAGS="$(CFLAGS)" all)

install-top:
	@if test -d $(TOPDIR); then : ; else mkdir $(TOPDIR); fi

install-bin: $(demo)
	@if test -d $(BINDIR); then : ; else mkdir $(BINDIR); fi
	chmod 0755 $(demo)
	$(INSTALL) $(demo) $(BINDIR)

install-man: 
	@(cd man; $(MAKE) install)

install-lib:
	@(cd src; $(MAKE) install)
	@(cd src/shared; $(MAKE) install)

install-library:
	@(cd library; $(MAKE) install)

install: install-top install-library install-bin install-lib install-man 

clean:
	@echo "cleaning in ./src"
	@(cd src; $(MAKE) clean)
	@echo "cleaning in ./src/shared"
	@(cd src/shared; $(MAKE) clean)
	@echo "cleaning in ./man"
	@(cd man; $(MAKE) clean)
	@echo "cleaning in ./library"
	@(cd library; $(MAKE) clean)
	$(RM) $(demo) *\~ "#"*

GENERATED_FILES = \
	config.status \
	src/bltConfig.h \
	Makefile \
	src/Makefile \
	src/TAGS \
	src/shared/Makefile \
	man/Makefile \
	library/Makefile \
	demos/*.ps

distclean: clean
	$(RM) $(GENERATED_FILES)

PUREFLAGS=
pure: normallib
	$(PURIFYHOME)/purify $(PUREFLAGS) \
	$(CC) $(CC_SWITCHES) $(srcdir)/tkAppInit.c -o $(demo) $(LIBRARIES)

