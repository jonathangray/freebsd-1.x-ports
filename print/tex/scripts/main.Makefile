# Set Definitions

## Methods
CC=gcc2 -O2
INSTALL=install -o gunther -g tex

## Constants
DVIDRIVER=dvieps
PSOUT=f
DEFDEV=EpsonMXFX
DEFRES=240
RESRES=120:240:262.9:288:345.6:414.72:497.664:597.1968
###comment out if you don't want multilingual TeX
GERMAN=1
###the formats and bases to make
FORMATS=tex.fmt latex.fmt
BASES=mf.base
###
MFSCRCOL=500
MFSCRROW=600

## Directories
BINDIR=/usr/local/bin
MANDIR=/usr/local/man
MANEXT=1
LIBROOT=/usr/local/lib/TeX
LIBROOT_USER=\$${HOME}/TeX

### System TeX library tree
TEXLIB=$(LIBROOT)/tex
MFLIB=$(LIBROOT)/mf
PSLIB=$(LIBROOT)/ps
DVILIB=$(LIBROOT)/dvi
FONTLIB=$(LIBROOT)/fonts
TEXINPUTS=$(TEXLIB)/inputs
TEXFORMATS=$(TEXLIB)/formats
BIBTEX=$(LIBROOT)/bibtex
MFINPUTS=$(MFLIB)/inputs
MFBASES=$(MFLIB)/bases
TFM=$(FONTLIB)/tfm
PK=$(FONTLIB)/pk
VF=$(FONTLIB)/vf
CMSRC=$(MFINPUTS)/CM

### User TeX library tree
TEXLIB_USER=$(LIBROOT_USER)/tex
MFLIB_USER=$(LIBROOT_USER)/mf
PSLIB_USER=$(LIBROOT_USER)/ps
DVILIB_USER=$(LIBROOT_USER)/dvi
FONTLIB_USER=$(LIBROOT_USER)/fonts
TEXINPUTS_USER=$(TEXLIB_USER)/inputs
TEXFORMATS_USER=$(TEXLIB_USER)/formats
BIBTEX_USER=$(LIBROOT_USER)/bibtex
MFINPUTS_USER=$(MFLIB_USER)/inputs
MFBASES_USER=$(MFLIB_USER)/bases
TFM_USER=$(FONTLIB_USER)/tfm
PK_USER=$(FONTLIB_USER)/pk
VF_USER=$(FONTLIB_USER)/vf

# Export Definitions
USRLIB= LIBROOT_USER TEXLIB_USER MFLIB_USER PSLIB_USER DVILIB_USER \
	FONTLIB_USER TEXINPUTS_USER TEXFORMATS_USER BIBTEX_USER \
	MFINPUTS_USER MFBASES_USER TFM_USER PK_USER VF_USER

SYSLIB= LIBROOT TEXLIB MFLIB PSLIB DVILIB FONTLIB TEXINPUTS \
	TEXFORMATS BIBTEX MFINPUTS MFBASES TFM PK VF CMSRC

EXPORT= CC INSTALL PSOUT DEFDEV DEFRES RESRES GERMAN FORMATS BASES MANEXT \
	MFSCRCOL MFSCRROW BINDIR MANDIR $(SYSLIB) $(USRLIB)

.if exists(Makefile.inc)
.include "Makefile.inc"
.endif

.ifdef GERMAN
TEXTRA=german-formats
.endif

# Definitions local to Makefile
TeX=TeX3.14
dvips=dvips-5.497
Xdvi=xdvi
dvi=beebe
dviutils=dvi2tty

ALL= TeX dvips Xdvi dvi dviutils
ALLDIR= $(TeX) $(dvips) $(Xdvi) $(dvi) $(dviutils)

# Build targets
all: Makefile.inc stamp-port $(ALL)

stamp-port:
	cd ./scripts ; \
	cp ./TeX.Makefile ../$(TeX)/Makefile ; \
	cp ./dvips.Makefile ../$(dvips)/Makefile ; \
	cp ./Xdvi.Imakefile ../$(Xdvi)/Imakefile ; \
	cp ./dvi.Makefile ../$(dvi)/Makefile ; \
	cp ./dviutils.Makefile ../$(dviutils)/Makefile ; \
	cp ./tex.convert ../$(TeX)/tex/convert ; \
	cp ./mf.convert ../$(TeX)/mf/convert ; \
	echo $(seddef) |sed -f mksedscript.sed > var.sed ; \
	sed -f ./var.sed ./Install_INPUTS.raw > ../$(TeX)/Install_INPUTS ; \
	sed -f ./var.sed ./site.h.raw > ../$(TeX)/site.h ; \
	sed -f ./var.sed ./config.ps.raw > ../$(dvips)/config.ps ; \
	sed -f ./var.sed ./MakeTeXPK.raw > ../$(dvips)/MakeTeXPK ; \
	sed -f ./var.sed ./Xdefaults.raw > ../usr/.Xdefaults-`hostname` ; \
	sed -f ./var.sed ./environ.sh.raw > ../usr/texsetup.sh ; \
	sed -f ./var.sed ./environ.csh.raw > ../usr/texsetup.csh ; \
	sed -f ./var.sed ./modes.mf.raw > ./modes.mf
	ln -f $(TeX)/mf/MFwindow/x11-Xlib.c $(TeX)/mf/MFwindow/x11.c 
	touch stamp-port

TeX: stamp-port stamp-inputs TeXprog $(TEXTRA)

TeXprog:
	cd $(TeX) ; \
	make $(makedef) all ; \
	make $(makedef) formats ; \
	make $(makedef) bases ; \
	make $(makedef) manpages

TeXpass:
	cd $(TeX) ; \
	make $(makedef) $(PASS)

dvips: stamp-port
	cd $(dvips) ; \
	make $(makedef) all	

Xdvi: stamp-port
	cd $(Xdvi) ;\
	xmkmf ;\
	make $(makedef) all

dvi: stamp-port
	cd $(dvi) ;\
	make $(makedef) $(DVIDRIVER)

dviutils: stamp-port
	cd $(dviutils) ;\
	make $(makedef) all

# Installation targets
stamp-inputs: stamp-tree
	cd $(TeX) ; \
	sh Install_INPUTS
	touch $(.TARGET)

install: $(ALL:S/^/install./g)

install.TeX: stamp-tree
	cd $(TeX) ; \
	make $(makedef) install ; \
	make $(makedef) install-formats ; \
	make $(makedef) install-bases ; \
	make $(makedef) install-manpages ; \

install.dvips: stamp-tree
	cd $(dvips) ; \
	make $(makedef) install

install.Xdvi:
	cd $(Xdvi) ;\
	make $(makedef) install ;\
	make $(makedef) install.man

install.dvi:
	cd $(dvi) ;\
	make -k $(makedef) install ;\

install.dviutils:
	cd $(dviutils) ;\
	make $(makedef) install ;\

# Cleanup targets
clean: $(ALL:S/^/clean./g)
	rm -f *~ #*# a.out *.o stamp-*
	cd ./scripts ; rm -f var.sed modes.mf
	rm -r -f ./usr/*

clean.TeX: stamp-tree
	cd $(TeX) ; \
	make $(makedef) clean ; \

clean.dvips: stamp-tree
	cd $(dvips) ; \
	make $(makedef) clean

clean.Xdvi:
	cd $(Xdvi) ;\
	make $(makedef) clean ;\

clean.dvi:
	cd $(dvi) ;\
	make $(makedef) clean ;\

clean.dviutils:
	cd $(dviutils) ;\
	make $(makedef) clean ;\

# Very-cleanup targets
veryclean: $(ALL:S/^/veryclean./g)
	rm -f stamp-* Makefile.inc *~ #*# a.out *.o 
	cd ./scripts ; rm -f var.sed modes.mf
	rm -r -f ./usr/*
	cd $(TeX) ; rm -f ./Makefile ./site.h ./Install_INPUTS \
			./mf/convert ./tex/convert 
	cd $(dvips) ; rm -f ./Makefile ./config.ps
	rm -f ./$(Xdvi)/Imakefile ./$(dvi)/Makefile ./$(dviutils)/Makefile

veryclean.TeX: stamp-tree
	cd $(TeX) ; \
	make $(makedef) veryclean ; \

veryclean.dvips: stamp-tree
	cd $(dvips) ; \
	make $(makedef) veryclean

veryclean.Xdvi:
	cd $(Xdvi) ;\
	make $(makedef) clean ;\

veryclean.dvi:
	cd $(dvi) ;\
	make $(makedef) veryclean ;\

veryclean.dviutils:
	cd $(dviutils) ;\
	make $(makedef) veryclean ;\

# Include file
Makefile.inc: Makefile
	@./scripts/mkdefines.sh -make makedef $(EXPORT) > Makefile.inc ; \
	echo >> Makefile.inc ; \
	./scripts/mkdefines.sh -sed seddef $(EXPORT) >> Makefile.inc ; \
	echo >> Makefile.inc ; \
	./scripts/mkdefines.sh -xpand syslib $(SYSLIB) >> Makefile.inc ; \
	echo >> Makefile.inc ; \
	./scripts/mkdefines.sh -xpand usrlib $(USRLIB) >> Makefile.inc ; \
	echo >> Makefile.inc ; \
	echo "Updating Makefile.inc please start make again" ; \
	exit 3

# System library tree
stamp-tree:
	for i in $(syslib) ; \
	do \
	  if [ ! -d $$i ] ; \
	  then \
	    mkdir -p $$i ; \
	  fi ; \
	done
	touch $(.TARGET)

# User library tree
user-tree:
	for i in $(usrlib:S/\${HOME}/.\/usr/g) ; \
	do \
	  if [ ! -d $$i ] ; \
	  then \
	    mkdir -p $$i ; \
	  fi ; \
	done

#Internationalization of formats, sorry, I only have a german tex so far...
##This defaults to english mode, use \germanTeX to switch to german.
GERMANIZE=\\input german.tex \
	\\input hyphen.german.tex \
	\\originalTeX

german-formats:
	cd $(TeX)/tex ; \
	./initex plain.tex $(GERMANIZE) \\dump ; \
	mv plain.fmt tex.fmt ; \
	./initex lplain.tex $(INTERNATIONALIZE) \\dump ; \
	mv lplain.fmt latex.fmt
