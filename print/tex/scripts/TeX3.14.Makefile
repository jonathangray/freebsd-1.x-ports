# Overall Makefile for making TeX, Metafont, and friends, using WEB to C.
 
# TeX 3.14 + Metafont 2.7 + web2c b.
# 
version=5.84b

# Directory that site.h is in relative to subdirectories, i.e., `..'
# means the directory containing this Makefile.
SITEDIR=..

# Default C compiler
CC=$(CC_386BSD)

# Default flags to give the C compiler.  (Don't define CFLAGS directly.)
# this is defined as part of 386BSD_CC
OPT=

# Default flags to give to the loader.
LDFLAGS=-L/usr/X386/lib

# Default libraries with which to link (libraries for the Metafont
# support are specified separately).
LOADLIBES=

# Make sure we use the sh.  If your sh is broken, you might try bash,
# the GNU replacement.
SHELL=/bin/sh

# The yacc program to use on web2c/web2c.yacc.
YACC=yacc

# The lex program to use on web2c/web2c.lex.
LEX=lex
# -ll for lex, -lfl for flex.
LEXLIB=-ll

# Need -D_POSIX_SOURCE here, perhaps, if on a POSIX system.
LEXCFLAGS=-D_POSIX_SOURCE

# How to make a link.
LN=ln -s

# Support libraries for Metafont.
x10lib=-lX
x11lib=-lXt -lX11
sunlib=-lsuntool -lsunwindow -lpixrect

# This line should select the libraries that match what you said in site.h.
wlibs=$(x11lib)

# These lines define the format and base files that `make fmts' and
# `make bases' will try to make, and `make install' will try to install.
# See README.W2C for a brief description of each of these formats.
formats=$(FORMATS_386BSD)
bases=$(BASES_386BSD)

# The name of the file that defines your local devices, for use with
# Metafont.  (Only relevant during `make bases'.)  I urge you to get the
# file `modes.mf', which defines all known modes, plus useful
# definitions for all fonts.  It's available from ftp.cs.umb.edu
# [192.12.26.23] as pub/tex/modes.mf, among other places.
localmodes=modes.mf

# You may need INSTALL=cp on system V, if you don't have GNU install. 
# Or you can install the programs by hand.
# $(fileinstall) is used for the format and base files, man files, and
# other non-executables, and $(programinstall) is used for
# the executables.
INSTALL=$(INSTALL_386BSD)
fileinstall=$(INSTALL) -m 664
proginstall=$(INSTALL) -m 775 -s

# These pathnames are used to customize the manual pages with local
# directory names; some of them are used in `make install'.  They should
# match the system directories in the paths defined in `./site.h'.
bindir=$(BINDIR_386BSD)
manext=$(MANEXT_386BSD)
mandir=$(MANDIR_386BSD)/man$(manext)

texlibdir=$(LIBROOT_386BSD)
texpooldir=$(TEXLIB_386BSD)
texinputdir=$(TEXINPUTS_386BSD)
texfontdir=$(FONTLIB_386BSD)
formatdir=$(TEXFORMATS_386BSD)

mflibdir=$(MFLIB_386BSD)
mfpooldir=$(MFLIB_386BSD)
mfinputdir=$(MFINPUTS_386BSD)
basedir=$(MFBASES_386BSD)

# Where the Computer Modern sources are.
# (The man pages claim that you have installed the standard utility
# sources, like `grayf.mf' and `expr.mf', somewhere in $(mfinputdir).)
cmsources=$(CMSRC_386BSD)

default: all


# Stuff below here probably doesn't need to be changed.
# 
alldirs=web tex mf texware mfware fontutil bibtex dviutil
triptrapdirs=tex texware mf mfware

makeargs=SITEDIR="$(SITEDIR)" CC="$(CC)" OPT="$(OPT)" \
SHELL="$(SHELL)" LDFLAGS="$(LDFLAGS)" LOADLIBES="$(LOADLIBES)" wlibs="$(wlibs)"

web2c=web2c
web2cprograms=$(web2c)/web2c $(web2c)/regfix $(web2c)/splitup \
              $(web2c)/fixwrites
commonsources=common/alloca.c common/extra.c common/main.c common/endian.c

triptrap: stamp-tangle stamp-common
	for name in $(triptrapdirs);					\
        do								\
          (cd $${name}; make $(makeargs) LN="$(LN)" triptrap);		\
        done  

run-triptrap: run-trip run-trap

clean-triptrap:
	cd tex; make veryclean
	cd mf; make veryclean

all:	stamp-web2c stamp-common
	for name in $(alldirs);						\
        do								\
          (cd $${name}; echo $${name}; make $(makeargs) all);		\
        done
	
TeX:	stamp-web2c stamp-tangle stamp-common
	cd tex; make $(makeargs) all

BibTeX: stamp-web2c stamp-tangle stamp-common
	cd bibtex; make $(makeargs) all

run-trip:
	cd tex; make run-trip

MF:	stamp-web2c stamp-tangle stamp-common
	cd mf; make $(makeargs) all

run-trap:
	cd mf; make run-trap


# The targets below must be up to date for anything to be made.
# 

stamp-web2c:
	cd $(web2c); \
          make $(makeargs) YACC="$(YACC)" LEX="$(LEX)" LEXLIB="$(LEXLIB)" all
	touch stamp-web2c

stamp-common: $(commonsources)
	cd common; make $(makeargs) all
	touch stamp-common

stamp-tangle: stamp-web2c stamp-common
	cd web; make $(makeargs) tangle
	touch stamp-tangle


manpages:
	cd man; make SITEDIR=$(SITEDIR)					\
		bindir=$(bindir) texlibdir=$(texlibdir)			\
		texpooldir=$(texpooldir) formatdir=$(formatdir)		\
		texinputdir=$(texinputdir) texfontdir=$(texfontdir)	\
		mflibdir=$(mflibdir) mfpooldir=$(mfpooldir)		\
		basedir=$(basedir) mfinputdir=$(mfinputdir)		\
                cmsources=$(cmsources)					\
                all

bases: stamp-bases
stamp-bases:
	cd mf; MFPOOL=.; export MFPOOL; \
          make bases="$(bases)" localmodes="$(localmodes)" bases
	touch stamp-bases

formats: stamp-formats
stamp-formats:
	cd tex; TEXPOOL=.; export TEXPOOL; make formats="$(formats)" fmts
	touch stamp-formats


# Installation targets.
# 
install-manpages:
	cd man; make mandir=$(mandir) INSTALL="$(fileinstall)" \
          SITEDIR="$(SITEDIR)" manext=$(manext) install

install:
	if test -d $(bindir); then exit 0; else mkdir $(bindir); fi
	$(proginstall) tex/initex $(bindir)/initex
	$(proginstall) tex/virtex $(bindir)/virtex
	$(proginstall) mf/inimf $(bindir)/inimf
	$(proginstall) mf/virmf $(bindir)/virmf
	$(proginstall) web/tangle $(bindir)/tangle
	$(proginstall) web/weave $(bindir)/weave
	$(proginstall) texware/dvitype $(bindir)/dvitype
	$(proginstall) texware/pltotf $(bindir)/pltotf
	$(proginstall) texware/tftopl $(bindir)/tftopl
	$(proginstall) mfware/gftodvi $(bindir)/gftodvi
	$(proginstall) mfware/gftopk $(bindir)/gftopk
	$(proginstall) mfware/gftype $(bindir)/gftype
	$(proginstall) mfware/mft $(bindir)/mft
	$(proginstall) fontutil/pktogf $(bindir)/pktogf
	$(proginstall) fontutil/pktype $(bindir)/pktype
	$(proginstall) fontutil/vftovp $(bindir)/vftovp
	$(proginstall) fontutil/vptovf $(bindir)/vptovf
	$(proginstall) bibtex/bibtex $(bindir)/bibtex
	$(proginstall) dviutil/dvicopy $(bindir)/dvicopy
	if test -d $(texpooldir); then exit 0; else mkdir $(texpooldir); fi
	-if test -s tex/tex.pool; \
        then (cd tex; $(fileinstall) tex.pool $(texpooldir) ); fi
	if test -d $(mfpooldir); then exit 0; else mkdir $(mfpooldir); fi
	-if test -s mf/mf.pool; \
        then (cd mf; $(fileinstall) mf.pool $(mfpooldir) ); fi

install-formats: stamp-formats
	if test -d $(formatdir); then exit 0; else -mkdir $(formatdir); fi
	for f in $(formats);						\
	do								\
	  rm -f $(bindir)/`basename $$f .fmt`;				\
          ln $(bindir)/virtex $(bindir)/`basename $$f .fmt`;		\
          $(fileinstall) tex/$$f $(formatdir);				\
	done
	cd $(formatdir); rm -f plain.fmt; ln tex.fmt plain.fmt

install-bases: stamp-bases
	if test -d $(basedir); then exit 0; else -mkdir $(basedir); fi
	for f in $(bases);						\
	do								\
	  rm -f $(bindir)/`basename $$f .base`;				\
	  ln $(bindir)/virmf $(bindir)/`basename $$f .base`;		\
          $(fileinstall) mf/$$f $(basedir);				\
	done
	cd $(basedir); rm -f plain.base; ln mf.base plain.base


# Need GNU tar for this.
# 
web2c-dist: web/tangleboot.pas
	@echo -n "check -linet;tangleboot"
	@echo "big{tex,mf.ch; trie change"
	mv ChangeLog ChangeLog.W2C
	cd ..;								\
	  find src-$(version) \! -name '*.web' -type f -print > /tmp/files; \
	  tar czfT web2c-$(version).tar.Z /tmp/files;			\
          ls -ld web*
	mv ChangeLog.W2C ChangeLog

web-dist: web/tangleboot.pas
	cd ..;								\
          find src-$(version) -name '*.web' -print > /tmp/webfest;	\
          tar czfT web-$(version).tar.Z /tmp/webfest;			\
          ls -ld web*

web/tangleboot.pas:
	cd web; make tangle.p; cp tangle.p tangleboot.pas; make veryclean

clean:
	rm -f stamp-*
	for name in web2c common $(alldirs);				\
        do								\
          (cd $${name}; make clean);					\
        done

veryclean:
	rm -f stamp-*
	for name in web2c common man $(alldirs);			\
        do								\
          (cd $${name}; make veryclean);				\
        done
	rm -f \#*\# *~ *.bak *.ckp core

.PHONY: default triptrap run-triptrap clean-triptrap all TeX run-trip \
	MF run-trap manpages bases formats install-manpages install \
        install-formats install-bases web2c-dist web-dist clean veryclean
