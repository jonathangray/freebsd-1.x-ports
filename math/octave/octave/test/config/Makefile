# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's test/config directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ../..

srcdir = .
VPATH = .

include $(TOPDIR)/Makeconf

SOURCES = unix-octave.exp

DISTFILES = Makefile.in $(SOURCES)

SUBDIRS =

all:
.PHONY: all

check:
.PHONY: check

install: all
.PHONY: install

uninstall:
.PHONY: uninstall

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

clean:
.PHONY: clean

mostlyclean:
.PHONY: mostlyclean

distclean: clean
	rm -f Makefile
.PHONY: distclean

realclean: distclean
	rm -f tags TAGS
.PHONY: realclean

local-dist:
	ln $(DISTFILES) ../../`cat ../../.fname`/test/config
.PHONY: local-dist

dist:
	ln $(DISTFILES) ../../`cat ../../.fname`/test/config
.PHONY: dist
