
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<
	-bibtex `basename $< .tex`
	-latex $<
	-latex $<

all: btxdoc.dvi btxhak.dvi

btxdoc.dvi: btxdoc.tex

btxhak.dvi: btxhak.tex

clean realclean:
	rm -f *.?? *.aux *.log *.bbl *.dvi *.blg
