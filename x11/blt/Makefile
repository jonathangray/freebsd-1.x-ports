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

TOPDIR	     = /usr/local/blt
BINDIR       = /usr/local/blt/bin

# ------------------------------------------------------------------------
#       Include directives for Tcl, Tk, and X include files 
# ------------------------------------------------------------------------

TCL_INCLUDE   = -I/usr/local/include
TK_INCLUDE    = -I/usr/local/include
X_INCLUDE     = -I/usr/X386/include

# ------------------------------------------------------------------------
#       Libraries directives for Tcl, Tk, X11, and BLT
# ------------------------------------------------------------------------

X_LIB        = -L/usr/X386/lib -lX11
TK_LIB       = -L/usr/local/lib -ltk
TCL_LIB      = -L/usr/local/lib -ltcl
BLT_LIB      = -L./src/shared -L./src -lBLT
EXTRA_LIBS   =  -lm #-R/usr/local/lib:/usr/local/lib:/usr/X386/lib


# ------------------------------------------------------------------------
#       Don't edit anything beyond this point
# ------------------------------------------------------------------------

demo 	     = blt_wish
INCLUDES     = -I. $(TK_INCLUDE) $(TCL_INCLUDE) $(X_INCLUDE)
CC_SWITCHES  = $(CFLAGS) $(DEFINES) $(INCLUDES)
LIBRARIES    = $(BLT_LIB) $(TK_LIB) $(TCL_LIB) $(X_LIB) $(EXTRA_LIBS)
INSTALL      = /usr/bin/install -c
RANLIB 	     = ranlib
SHELL 	     = /bin/sh
RM 	     = rm -f
srcdir	     = .

all: $(demo)

$(demo): normallib sharedlib
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

install: install-top install-library install-bin install-lib install-man \
	install-apps

package: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST -r pkg/+REQ blt_bin

clean:
	@echo "cleaning in ./src"
	@(cd src; $(MAKE) clean)
	@echo "cleaning in ./src/shared"
	@(cd src/shared; $(MAKE) clean)
	@echo "cleaning in ./man"
	@(cd man; $(MAKE) clean)
	@echo "cleaning in ./library"
	@(cd library; $(MAKE) clean)
	@(cd ./applications/kosherdill; $(MAKE) clean)
	@(cd ./applications/extloader; $(MAKE) clean)
	$(RM) $(demo) blt_bin.tgz *\~ "#"*

apps:
	@(cd ./applications/kosherdill; $(MAKE))
	@(cd ./applications/extloader; $(MAKE))

install-apps: apps
	@(cd ./applications/kosherdill; $(MAKE) install)
	@(cd ./applications/extloader; $(MAKE) install)

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

