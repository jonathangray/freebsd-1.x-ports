# ======================================================================
# Makefile for files in ~beebe/tex/tugboat
# This constructs the tugboat.bib file from the tb*.cnt files using
# the awk program tugboat.awk, and then produces complete test
# bibliographies in alpha and unsrt form.  A kwic index of the
# bibliography is also produced:
#
# Targets:
# all		tugbib + tugbib2 + kwic
# tugbib	bibliography in alpha order 
# tugbib2	bibliography in unsrt order 
# kwic		keyword-in-context index to tugboat.bib.  Must be printed
#		in landscape orientation.
# clean		remove files that can be recreated by running make again
# [29-Oct-88]
# ======================================================================

AWK = nawk

BIBTEX = nbibtex
BIBTEX = bibtex

DETEX = detex

DEV = alw

# We need a BIGG version of Common TeX to handle this big bibliography
# LATEX = cm-latex
LATEX = latex

RM = rm -f

TEX = tex

.SUFFIXES:

.SUFFIXES:	.aux .dvi-$(DEV) .dvi .spell .ltx .tex .kwic .ptx .bbl .bib

.bib.bbl:
	- $(NBIBTEX) $*

.bib.ptx:
	nawk -f kwic-bib.awk <$< | ptx -r -f >$@

.dvi.dvi-$(DEV):
	dvi$(DEV) -m$(MAG) $<
	cp $*.dvi-$(DEV) /$(DEV)
	rm -f $@

.ltx.aux .ltx.dvi:
	- $(LATEX) $<

.ltx.spell:
	$(DETEX) $< | spell >$@

.ptx.kwic:
	sed -f ptx.sed <$< >$@

.tex.aux .tex.dvi:
	$(TEX) $<

.tex.spell:
	$(DETEX) $< | spell >$@

TUGCONTENTS = tb0180.cnt tb0281.cnt tb0382.cnt tb0483.cnt tb0584.cnt \
	tb0685.cnt tb0786.cnt tb0887.cnt tb0988.cnt tb1089.cnt \
	tb1190.cnt tb1291.cnt

# For temporary debugging
# TUGCONTENTS = tb0988.cnt

all:	tugbib tugbib2 kwic

kwic:	kwic.dvi

kwic.dvi:	tugboat.kwic

tugboat.kwic:	tugboat.ptx ptx.sed

tugboat.ptx:	tugboat.bib kwic-bib.awk

tugbib:	tugbib.dvi

# tugbib is in alpha order; this is preferable to plain (which gives
# the same reference order) so we can test the correctness of alphanumeric
# bibliography labels.
tugbib.dvi:	tugbib.ltx tugboat.bib
	- $(RM) tugbib.bbl
	- $(RM) tugbib.aux
	- $(LATEX) tugbib.ltx
	- $(BIBTEX) tugbib
	- $(LATEX) tugbib.ltx
	- $(LATEX) tugbib.ltx

tugbib.bbl:	tugbib.aux tugboat.bib
	$(BIBTEX) tugbib

tugbib2:	tugbib2.dvi

# tugbib2 is in unsrt order for checking the bibliography against the
# original TUGboat tables of contents
tugbib2.dvi:	tugbib2.ltx tugboat.bib
	- $(RM) tugbib2.bbl
	- $(RM) tugbib2.aux
	- $(LATEX) tugbib2.ltx
	- $(BIBTEX) tugbib2
	- $(LATEX) tugbib2.ltx
	- $(LATEX) tugbib2.ltx

tugbib2.bbl:	tugbib2.aux tugboat.bib
	- $(BIBTEX) tugbib2

tugboat.bib:	$(TUGCONTENTS) tugboat.awk
#	$(AWK) -f tugboat.awk $(TUGCONTENTS) >tugboat.bib
	$(AWK) -f tugboat.awk $(TUGCONTENTS) | bibclean >tugboat.bib
	echo ==== Hand-edit tugboat.def to fix tail-end definitions ====

clean:
	@- $(RM) *.aux
	@- $(RM) *.blg
	@- $(RM) *.bbl
	@- $(RM) *.dvi*
	@- $(RM) *.log
	@- $(RM) *.lst
	@- $(RM) *.ptx
	@- $(RM) *.kwic
	@- $(RM) *~*
