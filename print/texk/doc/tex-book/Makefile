
.SUFFIXES:
.SUFFIXES: .dvi .ill

.ill.dvi:
	-printf "\n\n\n" | tex $<

all: texbook.dvi

texbook.dvi: texbook.ill

clean realclean:
	rm -f *.log *.dvi *.ill answers.tex index.tex
