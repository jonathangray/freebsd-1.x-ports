
.SUFFIXES:
.SUFFIXES: .dvi .texi .info

.texi.info:
	-makeinfo $<

.texi.dvi:
	-texi2dvi $<

all: texdraw.info texdraw.dvi

texdraw.info: texdraw.texi

texdraw.dvi: texdraw.texi

clean realclean:
	rm -f *.log *.aux *.toc *.dvi *.?? *.p?? *.cps *.info* *.fns
