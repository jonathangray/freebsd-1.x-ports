
.SUFFIXES:
.SUFFIXES: .dvi .texi .info

.texi.info:
	-makeinfo $<

.texi.dvi:
	-texi2dvi $<

all: kpathsea.dvi kpathsea.info

kpathsea.info: kpathsea.texi

kpathsea.dvi: kpathsea.texi

clean realclean:
	rm -f *.?? *.??? *.info*
