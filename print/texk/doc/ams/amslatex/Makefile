
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<
	-latex $<

all: amslatex.dvi

amslatex.dvi: amslatex.tex 

clean realclean:
	rm -f *.aux *.log *.dvi *.toc *.bbl *.blg
