NROFF=	nroff
TROFF=	troff
DITROFF=ditroff
NMACS=	../naa
TMACS=	../taa
DFLAGS=	-Pdp

.SILENT:

ref:
	$(NROFF) naa 0aa.nr 1aa.nr toc.nr
	chdir quel; $(NROFF) $(NMACS) *.nr
	chdir unix; $(NROFF) $(NMACS) *.nr
	chdir files; $(NROFF) $(NMACS) *.nr
	chdir error; $(NROFF) $(NMACS) *.nr

tref:
	$(TROFF) taa 0aa.nr 1aa.nr toc.nr
	chdir quel; $(TROFF) $(TMACS) *.nr
	chdir unix; $(TROFF) $(TMACS) *.nr
	chdir files; $(TROFF) $(TMACS) *.nr
	chdir error; $(TROFF) $(TMACS) *.nr

dref:
	$(DITROFF) $(DFLAGS) taa 0aa.nr 1aa.nr toc.nr
	chdir quel; $(DITROFF) $(DFLAGS) $(TMACS) *.nr
	chdir unix; $(DITROFF) $(DFLAGS) $(TMACS) *.nr
	chdir files; $(DITROFF) $(DFLAGS) $(TMACS) *.nr
	chdir error; $(DITROFF) $(DFLAGS) $(TMACS) *.nr
