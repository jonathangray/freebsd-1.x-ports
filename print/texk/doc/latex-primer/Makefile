
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<
	-latex $<

all: essential.dvi

essential.dvi: essential.tex

clean realclean:
	rm -f *.log *.aux  *.dvi
