# Generated automatically from Makefile.in by configure.
# ------------------------------------------------------------------------
# 	Makefile for library files and directories of BLT library
# ------------------------------------------------------------------------

INSTALL      = /bin/install -c
INSTALL_DATA = $(INSTALL) -m 644
BLT_LIBRARY  = /opt/cellar/deli/blt/library
RM           = rm -f
SHELL        = /bin/sh
FILES        = bltGraph.pro
srcdir	     = .

all:

install-top:
	@if test -d $(BLT_LIBRARY) ; then \
	    : ; \
	else \
	    mkdir $(BLT_LIBRARY) ; \
	fi

install-dragdrop: install-top
	@if test -d $(BLT_LIBRARY)/dd_protocols ; then \
	    : ; \
        else \
	    mkdir $(BLT_LIBRARY)/dd_protocols ; \
	fi
	@set -x; for i in $(srcdir)/dd_protocols/*tcl* ; do \
	    $(INSTALL_DATA) $$i $(BLT_LIBRARY)/dd_protocols ; \
	done 

install-files: install-top
	@set -x; for i in $(srcdir)/$(FILES) ; do \
	    $(INSTALL_DATA) $$i $(BLT_LIBRARY) ; \
	done 

install: install-dragdrop install-files

clean:
	$(RM) *\~ "#"*

