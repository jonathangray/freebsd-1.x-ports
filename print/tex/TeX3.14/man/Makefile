# Makefile for manual pages.
# 
# You need to change the variables and the pathnames only if this
# Makefile is invoked on its own, instead of from `../Makefile'.
#

# Directory that site.h is in.
# 
SITEDIR=..

# What to end the filenames with.
# 
manext=l


INSTALL=cp

# These pathnames are used to customize the manual pages with
# local directory names.  They should match the system directories
# in the paths defined in `../site.h'.
# 
bindir=/usr/local/bin
mandir=/usr/man/man$(manext)

texlibdir=/usr/local/lib/tex
texpooldir=$(texlibdir)
texinputdir=$(texlibdir)/macros
texfontdir=$(texlibdir)/fonts
formatdir=$(texlibdir)/formats

mflibdir=/usr/local/lib/mf
mfpooldir=$(mflibdir)
mfinputdir=$(mflibdir)/macros
basedir=$(mflibdir)/bases

cmsources=/src/TeX+MF/typefaces/cm

# Delete the man pages for programs you aren't installing.  (By default,
# the top-level `make install' does not install `patgen' or `pooltype'.)
# The programs are listed in alphabetical order.
# 
manfiles=		\
	bibtex.1	\
	dvitype.1	\
	gftodvi.1	\
	gftopk.1	\
	gftype.1	\
	latex.1		\
	mf.1		\
	mft.1		\
	pktogf.1	\
	pktype.1	\
	pltotf.1	\
	slitex.1	\
	tangle.1	\
	tex.1 		\
	tftopl.1	\
	vftovp.1	\
	vptovf.1	\
	weave.1

#	pooltype.1	\
#	patgen.1	\


# The edited file always has extension .1; we change it when we install.
# 
.SUFFIXES:	.man .1
.man.1:
		sed -f sedscript $*.man > $*.1

all: 		stamp-manpages

stamp-manpages:	$(manfiles)
		touch stamp-manpages

install: stamp-manpages
		-mkdir -p $(mandir)
		for nameone in $(manfiles);				\
	        do							\
                  name=`basename $${nameone} .1`;			\
        	  $(INSTALL) $${name}.1 $(mandir)/$${name}.$(manext);	\
	        done

clean:
		rm -f sedscript *.1 stamp-manpages

veryclean: clean


# We do not make a dependency on $(SITEDIR)/Makefile since not all
# people keep the Makefile in the same directory as site.h; furthermore,
# the top-level Makefile usually changes for reasons other than those
# that affect the man pages, so we don't actually lose much.
# 
sedscript: $(SITEDIR)/site.h
	sed -n -e '/^#define/s/#define[ 	][ 	]*\([A-Z_a-z][A-Z_a-z]*\)[ 	][ 	]*\(.*\)/s;@\1@;\2;/p' $(SITEDIR)/site.h \
          | sed -e s/\\\"//g > $@
	echo 's;@BINDIR@;$(bindir);' >> $@
	echo 's;@TEXLIBDIR@;$(texlibdir);' >> $@
	echo "s;@TEXPOOLDIR@;$(texpooldir);" >> $@
	echo 's;@FORMATDIR@;$(formatdir);' >> $@
	echo 's;@TEXINPUTDIR@;$(texinputdir);' >> $@
	echo 's;@TEXFONTDIR@;$(texfontdir);' >> $@
	echo 's;@MFLIBDIR@;$(mflibdir);' >> $@
	echo 's;@MFPOOLDIR@;$(mfpooldir);' >> $@
	echo 's;@BASEDIR@;$(basedir);' >> $@
	echo 's;@MFINPUTDIR@;$(mfinputdir);' >> $@
	echo 's;@CMSOURCES@;$(cmsources);' >> $@

$(manfiles):	sedscript

bibtex.1:	bibtex.man
dvitype.1:	dvitype.man
gftodvi.1:	gftodvi.man
gftopk.1:	gftopk.man
gftype.1:	gftype.man
latex.1:	latex.man
mf.1:		mf.man
mft.1:		mft.man
patgen.1:	patgen.man
pktogf.1:	pktogf.man
pktype.1:	pktype.man
pltotf.1:	pltotf.man
pooltype.1:	pooltype.man
slitex.1:	slitex.man
tangle.1:	tangle.man
tex.1:		tex.man
tftopl.1:	tftopl.man
vftovp.1:	vftovp.man
vptovf.1:	vptovf.man
weave.1:	weave.man
