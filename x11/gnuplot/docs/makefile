#
# $Id: makefile,v 1.1 1994/06/01 06:51:45 asami Exp $
#
#
# Makefile for GNUPLOT documentation
#
# Note that the top-level file for documentation is gnuplot.doc.
# See README.
#
# To print manual:
#    make gnuplot.dvi             (for latex)
#    (print or view gnuplot.dvi)
# OR
#    make gnuplot.nroff           (for nroff)
#    (print or view gnuplot.nroff)
#   or
#    make "TROFF=itroff" troff    (for troff; use your troff here)
#
# $Id: makefile,v 1.1 1994/06/01 06:51:45 asami Exp $

# usually overridden by ../Makefile
HELPDEST = /usr/local/lib

# substitute your troff command (and any flags) for this one
TROFF=troff

# substitute cp if you do not have the install program
INSTALL=install

# substitute your DVI to PostScript conversion program here
DVIPS=dvips

# Compiler flags
# -DSYSV if att sys V
# -DMSDOS if MSDOS PS
# -traditional -g -O if gcc (set 'CC = gcc')
# no extra flags for BSD
CFLAGS = -s
CC = cc

# Default information
help:
	@echo "Please do a 'make <TARGET>' where <TARGET> is one of" \
                "the following:"
	@echo
	@echo "check        check the gnuplot.doc file"
	@echo "clean        remove all derived files"
	@echo "dvi          DVI files (gnuplot.dvi gpcard.dvi)"
	@echo "gih          Unix and MSDOS help file (gnuplot.gih)"
	@echo "help         make help"
	@echo "hlp          VMS help file (gnuplot.hlp)"
	@echo "info         Info documentation (gnuplot.info)"
	@echo "install-unix Unix and MSDOS install"
	@echo "install-vms  VMS install"
	@echo "nroff        nroff documentation (gnuplot.nroff)"
	@echo "ps           PostScript files (gnuplot.ps gpcard.ps)"
	@echo "tex          LaTeX documentation (gnuplot.tex)"
	@echo "troff        troff documentation"
	@echo "tutorial     LaTeX tutorial (latextut/tutorial.dvi)"
	@echo
	@echo "If you are not familiar with makefiles or just want" \
                "to know what"
	@echo "'make <TARGET>' would do without actually doing" \
                "anything, then type"
	@echo "        'make -n <TARGET>'"
	@echo

# default is what is needed for interactive gnuplot
default: gnuplot.hlp gnuplot.gih

### [tn]roff documentation
troff: gnuplot.ms titlepag.ms
	tbl gnuplot.ms | eqn | $(TROFF) -ms

nroff: gnuplot.nroff

gnuplot.nroff: gnuplot.ms titlepag.ms
	tbl gnuplot.ms | neqn | nroff -ms | col > gnuplot.nroff

ms: gnuplot.ms

gnuplot.ms: doc2ms gnuplot.doc
	./doc2ms gnuplot.doc gnuplot.ms

doc2ms: doc2ms.c
	$(CC) $(CFLAGS) -o doc2ms doc2ms.c

### LaTeX documentation
tex: gnuplot.tex

gnuplot.tex: doc2tex gnuplot.doc
	./doc2tex gnuplot.doc gnuplot.tex

# this is how to make DVI files
dvi: gnuplot.dvi gpcard.dvi

gnuplot.dvi: gnuplot.tex titlepag.tex toc_entr.sty
	latex gnuplot
	latex gnuplot

gpcard.dvi: gpcard.tex
	tex gpcard

# this is how to make PostScript files
# if pslatex has been installed, add "times" to titlepage.tex
ps: gnuplot.ps gpcard.ps

gnuplot.ps: gnuplot.dvi
	$(DVIPS) gnuplot

gpcard.ps: gpcard.dvi
	$(DVIPS) gpcard

doc2tex: doc2tex.c
	$(CC) $(CFLAGS) -o doc2tex doc2tex.c

# this is how to make gnuplot.hlp
hlp: gnuplot.hlp

gnuplot.hlp: doc2hlp gnuplot.doc
	./doc2hlp gnuplot.doc gnuplot.hlp

doc2hlp: doc2hlp.c
	$(CC) $(CFLAGS) -o doc2hlp doc2hlp.c

# this is how to make gnuplot.gih
gih: gnuplot.gih

gnuplot.gih: doc2gih gnuplot.doc
	./doc2gih gnuplot.doc gnuplot.gih

doc2gih: doc2gih.c
	$(CC) $(CFLAGS) -o doc2gih doc2gih.c

# this is how to make Info documentation
info: gnuplot.info

gnuplot.info: gnuplot.doc
	perl doc2info.pl gnuplot.doc > gpltinfo.tex
	makeinfo +fill-column 80 gpltinfo.tex
	rm -f gpltinfo.tex

tutorial: latextut/tutorial.tex
	( cd latextut; $(MAKE) )

# this is how to check the gnuplot.doc file
check: checkdoc gnuplot.doc
	./checkdoc < gnuplot.doc

checkdoc: checkdoc.c
	$(CC) $(CFLAGS) -o checkdoc checkdoc.c

# For Unix and MSDOS only
install-unix: gnuplot.gih
	$(INSTALL) gnuplot.gih $(HELPDEST)

# for VMS only
install-vms: gnuplot.hlp
	$(INSTALL) gnuplot.hlp $(HELPDEST)

# remove all derived files
clean:
	rm -f doc2ms gnuplot.nroff gnuplot.ms \
              doc2tex gnuplot.tex gnuplot.dvi \
              gnuplot.aux gnuplot.log gnuplot.toc \
              gnuplot.ps gpcard.dvi gpcard.log gpcard.ps \
              doc2hlp gnuplot.hlp \
              doc2gih gnuplot.gih \
              checkdoc *~ *.o core a.out \
              gnuplot.info* gpltinfo.tex
	( cd latextut; $(MAKE) clean )
