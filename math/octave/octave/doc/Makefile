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

SOURCES =

TEXINFO = ack.texi amuse.texi arith.texi bugs.texi bugs1.texi \
	control.texi cp-idx.texi diffeq.texi expr.texi extend.texi \
	fcn.texi fn-idx.texi gpl.texi help.texi history.texi \
	hsuser.texi install.texi install1.texi intro.texi invoke.texi \
	io.texi linalg.texi matrix.texi nonlin.texi octave.texi \
	op-idx.texi optim.texi plot.texi program.texi quad.texi \
	rluser.texi signal.texi special.texi stats.texi stmt.texi \
	strings.texi system.texi using.texi var.texi vr-idx.texi

FORMATTED =  octave.dvi octave.ps octave.info octave.info-[0-9]*

DISTFILES = Makefile.in $(TEXINFO) $(FORMATTED) texinfo.tex

SPELL = $(patsubst %.texi, %.spell, $(TEXINFO))

%.spell : %.texi
	rm -f $@
	sed -e 's/@@/ at /g' -e 's/@[a-zA-Z]*//g' $< | spell > $@.tmp
	mv $@.tmp $@

all: octave.info octave.dvi octave.ps
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
	rm -f $(infodir)/octave.info*
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

doc-dist: all
	ln $(FORMATTED) ../`cat ../.fname`/doc
.PHONY: doc-dist
