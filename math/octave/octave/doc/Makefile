# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's doc directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ..

srcdir = .
VPATH = .

include $(TOPDIR)/Makeconf

MANFILES = # octave.1 # Don't distribute this -- it's way out of date.

SOURCES =

TEXINFO = ack.texi bugs.texi fn-idx.texi install.texi invoke.texi \
	vr-idx.texi bugs1.texi fcn.texi install1.texi octave.texi \
	cp-idx.texi gpl.texi intro.texi var.texi \
	expr.texi stmt.texi rluser.texi texinfo.tex

SPELL = $(patsubst %.texi, %.spell, $(TEXINFO))

DISTFILES = Makefile.in $(TEXINFO) $(MANFILES) octave.info*
# Maybe distribute these someday, but not now.
#	octave.dvi octave.ps

%.spell : %.texi
	rm -f $@
	sed -e 's/@@/ at /g' -e 's/@[a-zA-Z]*//g' $< | spell > $@.tmp
	mv $@.tmp $@

all: octave.info # octave.dvi octave.ps
.PHONY: all

octave.info: $(TEXINFO)
	-makeinfo -I$(srcdir) octave.texi

octave.dvi: $(TEXINFO)
	texi2dvi $(srcdir)/octave.texi

octave.ps: octave.dvi
	dvips -o octave.ps octave.dvi

../INSTALL.OCTAVE: install1.texi install.texi
	-makeinfo --no-header -I$(srcdir)/doc install1.texi
	-mv INSTALL ../INSTALL.OCTAVE

../BUGS: bugs1.texi bugs.texi
	-makeinfo --no-header -I$(srcdir)/doc bugs1.texi
	-mv BUGS ../BUGS

check: all
.PHONY: check

install: all
	if test -d $(infodir) ; then true ; \
	else $(TOPDIR)/mkpath $(infodir) ; fi
	if test -d $(infodir) ; then \
	  rm -f $(infodir)/octave.info* ; \
	  if test -f octave.info ; then \
	    for f in octave.info* ; do \
	      $(INSTALL_DATA) $$f $(infodir)/$$f ; \
	    done ; \
	  else \
	    for f in $(srcdir)/octave.info* ; do \
	      $(INSTALL_DATA) $$f $(infodir)/`basename $$f` ; \
	    done ; \
	  fi ; \
	fi
.PHONY: install

uninstall:
	for f in $(MANFILES) ; do \
		rm -f $(mandir)/`basename $$f 1`$(manext) ; \
	done
.PHONY: uninstall

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

spell: $(SPELL)
.PHONY: spell

clean:
	rm -f *.info *.info-* octave.cp octave.fn octave.ky \
	octave.pg octave.tp octave.vr octave.aux octave.log \
	octave.toc octave.dvi octave.ps
.PHONY: clean

mostlyclean:
	rm -f *.info *.info-*
.PHONY: mostlyclean

distclean: clean
	rm -f Makefile
.PHONY: distclean

realclean: distclean
	rm -f tags TAGS
.PHONY: realclean

local-dist: all
	ln $(DISTFILES) ../`cat ../.fname`/doc
.PHONY: local-dist

dist: all
	ln $(DISTFILES) ../`cat ../.fname`/doc
.PHONY: dist