
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<
	-latex $<

all: supertabular.dvi

supertabular.dvi: supertabular.tex

clean realclean:
	rm -f *.aux *.toc *.log *.dvi
