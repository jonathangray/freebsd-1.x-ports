RM	=	/bin/rm -f
SHELL	=	/bin/sh

.SUFFIXES:
.SUFFIXES: .dvi .tex

.tex.dvi:
	-latex $<

all:	ind.dvi

ind.dvi: ind.tex

clean mostlyclean:
	$(RM) core
	$(RM) *~
	$(RM) \#*
	$(RM) ind.dvi
	$(RM) ind.log

clobber distclean realclean reallyclean:	clean
