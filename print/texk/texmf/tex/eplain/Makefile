# Makefile for eplain.

version = 2.6

# For `install'.
prefix = /usr/local
bindir = $(prefix)/bin
texlibdir = $(prefix)/lib/texmf
texformatdir = $(texlibdir)/ini
texmacrodir = $(texlibdir)/tex/plain
infodir = $(prefix)/info

INITEX = initex
INSTALL_DATA = install -m 0664


default: eplain.tex

# Without etex.aux, \@citewarning is dumped as false, and so warnings
# about undefined labels are never issued.
etex.fmt: eplain.tex
	rm -f etex.aux
	cp /dev/null etex.aux
	$(INITEX) \&plain etex | grep -v '^\\font'

# We put several files together to make `eplain.tex'.  The shell script
# `merge' does everything.  
eplain.tex: merge arrow.tex btxmac.tex xeplain.tex *.sty
	version=$(version) ./merge

install: etex.fmt
	$(INSTALL_DATA) eplain.tex $(texmacrodir)
	$(INSTALL_DATA) texnames.sty $(texmacrodir)
	$(INSTALL_DATA) path.sty $(texmacrodir)
	$(INSTALL_DATA) etex.fmt $(texformatdir)
	cd $(texformatdir); rm -f eplain.fmt; ln etex.fmt eplain.fmt
	cd $(bindir); rm -f etex; ln virtex etex
	cd doc; $(MAKE) install

distdir = eplain-$(version)
topfiles = C* M* N* P* R* merge *.tex *.sty
dist: eplain.tex
	rm -rf $(distdir)
	mkdir $(distdir)
	ln $(topfiles) $(distdir)
	rm $(distdir)/eplain.tex
	cd $(distdir); $(MAKE) eplain.tex
	cp -p $(HOME)/bin/fix-checksum $(distdir)
	cp -p $(HOME)/bin/add-date $(distdir)
	cp -p $(HOME)/bin/add-version $(distdir)
	mkdir $(distdir)/doc $(distdir)/test
	cd doc; $(MAKE) version=$(version) dist
	ln test/*.tex test/*.bib $(distdir)/test
	GZIP=-9 tar czf $(distdir). $(distdir)
# Put the new info files in the local directory.
	cp $(distdir)/doc/eplain.info* $(info)
	rm -rf $(distdir)

# Make the DOS distribution.  This uses the environment variable `dos'.
dos: clean eplain.tex
	if [ ! -d $(dos)/eplain ];					\
        then mkdir $(dos)/eplain;					\
        else rm -f $(dos)/eplain/*;					\
        fi
	for f in $(topfiles);						\
        do								\
          lef < $$f > $(dos)/eplain/$$f;				\
        done
	lef < doc/eplain.texi > $(dos)/eplain/eplain.txi
	lef < doc/arrow.texi > $(dos)/eplain/arrow.txi
	add-version $(version) $(dos)/eplain/eplain.txi
	add-date $(dos)/eplain/eplain.txi
	cd $(dos)/eplain;						\
          mv ChangeLog change.log;					\
          mv Makefile makefile;						\
          mv PROJECTS projects;						\
	  mv README readme

mostlyclean clean:
	rm -f *.fmt *.log *.dvi
	rm -f test/*.dvi test/*.log
	rm -rf eplain-*
	(cd doc; $(MAKE) clean)

distclean: clean
	(cd doc; $(MAKE) distclean)

realclean: distclean
	rm -f eplain.tex INSTALL
	(cd doc; $(MAKE) realclean)
