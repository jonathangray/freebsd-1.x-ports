
.SUFFIXES:
.SUFFIXES: .dvi .ill

.ill.dvi:
	-printf "\n\n\n" | tex $<

all: mfbook.dvi

mfbook.dvi: mfbook.ill

clean realclean:
	rm -f *.log *.dvi *.ill answers.tex index.tex
