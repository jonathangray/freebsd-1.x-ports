# Makefile for the various bits of Eplain's documentation.

# For `install'.
prefix = /usr/local
infodir = $(prefix)/info

INSTALL_DATA = install -m 0664
MAKEINFO = makeinfo
MAKEINFO_FLAGS = --paragraph-indent=2 -I$(gnuorg)
gnuorg = $(HOME)/gnu/gnuorg
# That -I is purely for my own benefit in doing `make dist'.  It won't
# hurt anything for you (I hope).
TEX = TEXINPUTS=..:$(gnuorg): tex '\nonstopmode \input'
TEXINDEX = texindex

all: eplain.info eplain.dvi

eplain.dvi: eplain.cps
	$(TEX) eplain.texi
eplain.cps: eplain.cp
	$(TEXINDEX) eplain.??
eplain.aux eplain.cp: *.texi
	$(TEX) eplain.texi

info eplain.info: *.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) eplain.texi

install: eplain.info
	for i in eplain.info*; do $(INSTALL_DATA) $$i $(infodir); done

update:
	emacs -batch -l update.el

# Separate the installation instructions into a separate file, for the
# benefit of people who don't want to look at the info file.
INSTALL: *.texi
	$(MAKEINFO) $(MAKEINFO_FLAGS) --no-headers eplain.texi -o - \
	  | sed -n '/^Installation/,/^Invoking/'p \
	  | sed '$$d' > INSTALL

# `make dist' should only be called from the top-level Makefile, as it
# depends on $(version), at least.
distdir = ../eplain-$(version)/doc
dist:
	cp -p $(texsys)/bibtex/doc/btxdoc.tex $(distdir)
	cp -p $(plain)/texinfo.tex $(distdir)
	cp -p $(HOME)/gnu/gnuorg/*.texi $(distdir)
	ln Makefile *.texi eplain.??s $(distdir)
	(cd $(distdir); \
         add-version $(version) eplain.texi; \
         add-date eplain.texi; \
         $(MAKE) INSTALL eplain.info eplain.cps distclean)
	mv $(distdir)/INSTALL $(distdir)/..

clean mostlyclean:
	rm -f eplain.?? *.dvi *.log

distclean: clean
	for f in eplain.??s; do if test ! -s $$f; then rm -f $$f; fi; done

realclean: distclean
	rm -f *.info* eplain.??? INSTALL
