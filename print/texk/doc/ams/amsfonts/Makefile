
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-tex $<

all: userdoc.dvi

userdoc.dvi: userdoc.tex

clean realclean:
	rm -f *.aux *.log *.dvi *.??
