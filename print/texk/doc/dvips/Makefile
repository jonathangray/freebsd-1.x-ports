
.SUFFIXES:
.SUFFIXES: .dvi .texi .info

.texi.info:
	-makeinfo $<

.texi.dvi:
	-texi2dvi $<

all: dvips.info dvips.dvi

dvips.info: dvips.texi

dvips.dvi: dvips.texi

clean realclean:
	rm -f *.dvi *.?? *.aux *.bbl *.log *.toc *.info*
