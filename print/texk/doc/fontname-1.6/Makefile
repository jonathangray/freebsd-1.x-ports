# Makefile for the fontname document.

version = 1.6

# For make install.
prefix = /usr/gnu
infodir = $(prefix)/info

INSTALL = install -c
INSTALL_DATA = $(INSTALL) -m 664

MAKEINFO = makeinfo
MAKEINFO_FLAGS = --paragraph-indent=2

TEXI2DVI = texi2dvi

SHELL = /bin/sh

doc = fontname

all: $(doc).info dvi

dvi $(doc).dvi:
	$(TEXI2DVI) $(doc).texi

$(doc).info: $(doc).texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) ${.ALLSRC} -o $@

install: info
	for i in $(doc).info*; do $(INSTALL_DATA) $$i $(infodir)/$$i; done

# Prevent GNU make 3 from overflowing arg limit on system V.
.NOEXPORT:

distdir = fontname-$(version)
distfiles = ChangeLog Makefile README *.texi *.txt *.c *.h

dist:
	rm -rf $(distdir)
	mkdir $(distdir)
	ln $(distfiles) $(distdir)
	cp -p $(plain)/texinfo.tex $(distdir)
#
# Have to add version number and date before making the Info file.
	(cd $(distdir); \
         add-version $(version) $(doc).texi; \
         add-date $(doc).texi; \
         fix-checksum $(doc).texi; \
 	 $(MAKE) $(doc).dvi $(doc).info distclean)
#
# Put the new info files in the local directory.
	cp $(distdir)/fontname.info* $(info)
	GZIP=-9 tar czf $(distdir). $(distdir)
	rm -rf $(distdir)

mostlyclean clean:
	rm -f $(doc).?? *.dvi *.log *.toc

distclean: clean
	for f in $(doc).??s; do if test ! -s $$f; then rm -f $$f; fi; done

extraclean: distclean
	rm -f *~ \#* patch* *.orig *.rej *.bak *.ckp core a.out

realclean: distclean
	rm -f *.info* $(doc).???
