
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-tex $<

all: gentle-introduction.dvi

gentle-introduction.dvi: gentle-introduction.tex

clean realclean:
	rm -f *.dvi *.log
