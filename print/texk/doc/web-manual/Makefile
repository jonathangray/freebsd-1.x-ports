
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-tex $<

all: webman.dvi

webman.dvi: webman.tex

clean realclean:
	rm -f *.dvi *.log
