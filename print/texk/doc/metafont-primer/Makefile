
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<
	-latex $<
	-latex $<

all: metafont-for-beginners.dvi mfbegin.dvi

metafont-for-beginners.dvi: metafont-for-beginners.tex

mfbegin.dvi: mfbegin.tex

clean realclean:
	rm -f *.aux *.log *.toc *.dvi
