
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<
	-latex $<

all: wots.dvi

wots.dvi: wots.tex

clean realclean:
	rm -f *.aux *.log *.dvi
