
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<
	-latex $<

all: etexkomp.dvi

etexkomp.dvi: etexkomp.tex

clean realclean:
	rm -f *.aux *.log *.??
