
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-amslatex $<
	-amslatex $<
	-amslatex $<

all: amshelp.dvi

amshelp.dvi: amshelp.tex

clean realclean:
	rm -f *.dvi *.aux *.log *.toc
