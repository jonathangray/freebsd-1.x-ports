
.SUFFIXES:
.SUFFIXES: .dvi .texi .info

.texi.info:
	-makeinfo $<

.texi.dvi:
	-texi2dvi $<

all: latex.info
# latex.dvi

latex.info: latex.texi

latex.dvi: latex.texi

clean realclean:
	rm -f *.info* *.log *.aux *.dvi *.??
