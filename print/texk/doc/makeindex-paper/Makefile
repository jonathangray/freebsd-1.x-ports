LATEX	=	latex
RM	=	/bin/rm -f
SHELL	=	/bin/sh

all:	ind.dvi

ind.dvi:	ind.tex fig1.tex
	$(LATEX) ind

clean mostlyclean:
	$(RM) core
	$(RM) *~
	$(RM) \#*
	$(RM) ind.dvi
	$(RM) ind.log

clobber distclean realclean reallyclean:	clean
