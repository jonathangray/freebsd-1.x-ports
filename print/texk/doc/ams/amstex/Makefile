
.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-tex $<

all: amsguide.dvi joyerr.dvi 

amsguide.dvi: amsguide.tex

amstinst.dvi: amstinst.tex

joyerr.dvi: joyerr.tex

clean realclean:
	rm -f *.aux *.log *.dvi

