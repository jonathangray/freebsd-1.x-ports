# Generated automatically from Makefile.in by configure.
# ------------------------------------------------------------------------
#       Makefile for demonstation shell with extension loader
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
#       C Compiler options 
# ------------------------------------------------------------------------

LDFLAGS      = 
DEFINES      = 
CDEBUGFLAGS  = -O 
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
#       Libraries directives for Tcl, Tk, and X11
# ------------------------------------------------------------------------

X_LIB        = -L/usr/X11/lib -lX11
TK_LIB       = -L/opt/cellar/tk/tk3.6/lib -ltk
TCL_LIB      = -L/opt/cellar/tcl/tcl7.3/lib -ltcl
EXTRA_LIBS   =  -lnsl -lm -ldl

# ------------------------------------------------------------------------
#       Don't edit anything beyond this point
# ------------------------------------------------------------------------

OBJS	     = tkAppInit.o ext.o

program      = wish
INCLUDES     = -I. $(TK_INCLUDE) $(TCL_INCLUDE) $(X_INCLUDE)
CFLAGS 	     = $(CDEBUGFLAGS) $(DEFINES) $(INCLUDES)
LIBRARIES    = $(TK_LIB) $(TCL_LIB) $(X_LIB) $(EXTRA_LIBS)
INSTALL      = /bin/install -c
RANLIB 	     = @RANLIB@
SHELL 	     = /bin/sh
RM 	     = rm -f
MAN_SECT     = n
TOPDIR       = /opt/cellar/deli/blt
MANDIR       = /opt/cellar/deli/blt/man
SECT_DIR     = $(MANDIR)/man$(MAN_SECT)
srcdir	     = .

all: $(program)

$(program): $(OBJS)
	$(CC) $(LDFLAGS) $(CFLAGS) $(OBJS) -o $(program) $(LIBRARIES)
	@echo "make finished on `date`"

install: install-dirs $(program) install-man
	chmod 0755 $(program)
	$(INSTALL) $(program) $(BINDIR)

install-man: install-dirs
	@for i in $(srcdir)/*.man ; do \
	    file=$(SECT_DIR)/`basename $$i .man`.$(MAN_SECT) ; \
	    echo "installing manual $$file" ; \
	    rm -f $$file ; \
	    sed -e "/man\.macros/r $(srcdir)/man.macros" -e '/man\.macros/d' \
		$$i > $$file ; \
	    chmod 444 $$file; \
	done 

install-dirs:
	@for i in $(TOPDIR) $(BINDIR) $(MANDIR) $(SECT_DIR) ; do \
	    if test ! -d $$i ; then \
		mkdir $$i ; \
	    fi ; \
        done

clean:
	$(RM) $(program) $(OBJS) *\~ "#"*

GENERATED_FILES = config.status Makefile 

distclean: clean
	$(RM) $(GENERATED_FILES)
