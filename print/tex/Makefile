###########################################################################
# Set Definitions
#
###########################################################################
## Methods
##

### how to call the compiler, including optimization or debug
CC=gcc -O2
#-m486

### how to install files (stripped, and mode will be selected
### automaticaly) 
INSTALL=install -c -o bin -g bin

###########################################################################
## Default settings
##
## Coose your default printer device and gather the following
## information:
##
## (1) the name by which your device is supported in ./scripts/modes.mf
## (2) the resolution that your device prints, horizontal and vertical
##     resolution sometimes differ. In these cases you need both values.
##

### For METAFONT/modes.mf/mftopk: which is the default device? There
### are a lot, see ./scripts/modes.mf.raw for what you have.
DEFDEV=CanonCX

### still for METAFONT: default window geometry for online graphics
### output. (Normally needs not to be changed)
MFSCRCOL=500
MFSCRROW=600

### For ANY DVI-DRIVER (Beebe, dvips and xdvi): The default resolution
### given in dpi
### Note: Since we are using the default Postscript convertions, 300 DPI
### is the standard laser printer resolution that MetaFont can create
DEFRES=300

### For DVIPS and XDVI: The "last resort" fontsizes, if no better PK,
### GF or PXL font was found, try these sizes. This isn't really
### importent. Good values are the resolution of your device (possibly
### in different graphic modes) and maybe the standard magnifications
###
### resolution*(1.2**n) ; n: 1..5
###
RESRES=360:432

### For XDVI: the default aspect ratio, given in vertical resolution
### devided by horizontal resolution and multiplied with 1000. (eg.
### EpsonMXFX has 240dpi horiz. and 216dpi vertic. resolution.
### 216/240=0.9, times 1000 is 900)
DEFAR=900

### For DVIPS: which output method shall be used. You don't really 
### want to change this, since it's the filter mode PSOUT=f which
### anything else relies on
PSOUT=

###########################################################################
## What to make
##

### For TeX: The formats to make by default, why not just leave it
### unchanged? 
FORMATS=tex.fmt latex.fmt

### For METAFONT: The base(s) to make by default, I feel no need of
### having an extra cm.base, since mftopk calls only for mf (plain)
### and cm.mf is then loaded by \input, which is no disadvantage
#### BASES=mf.base cm.base
BASES=mf.base

### Which BEEBEdriver(s) to build. This can be any of the following:
###	dvialw	dvibit	dvican	dvie72	dvieps	dvigd	dviimp
###	dvijep	dvijet	dvil3p	dvil75	dvim72	dvimac	dvimpi
###	dvioki  dviprx	dvitos	dvio72  all     none
BEEBEDRV=dvieps dvil3p

### INTERNATIONAL TeX:
### comment out if you don't want multilingual TeX, currently there is
### only german installed. I cannot test the other languages. If you
### like, see in TeX3.14/contrib/tex/hyphen for your favourit 
### language's hyphenation and try it out! Is there a special macro
### file, such as german.tex? Please tell me what you've done to
### implement your language! Comment out if you don't want german
### style.
GERMAN=1

###########################################################################
## Directories
##
### Where to install binaries
BINDIR=/usr/local/bin

### Where to install man pages
MANDIR=/usr/local/man
MANEXT=1

### Where the system's TeX library tree is rooted
LIBROOT=/usr/local/lib/TeX

### Where the user's private TeX library tree is rooted. A user's TeX
### library tree is required if you want to work with mftopk. There is no
### reason why you want to kill this feature. This is nothing more than a
### default, any user remains free to organize his TeX tree differently.
LIBROOT_USER=\$${HOME}/TeX

### System TeX library tree structure. I feel this to be more
### consistent than what is often proposed.
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
CMSRC=$(MFINPUTS)/cm

### User's private TeX library tree structure, currently it reflects
### exactly the system's library tree structure. This may seem 
### overorganized for the small amount of TeX related files the
### average user deals with. But it's save for the default to have
### anything in reserve.
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

###########################################################################
# Export Definitions 
# 
# you only need to change this if you add new definitions above, which 
# you want to export to newly installed utilities, or newly implemented 
# features.  
# 
# The naming conventions are as follows: 
# 
# in the definition section 
# 
# <name>= <value> 
# 
# add to the export list 
# 
# EXPORT= ... <name>  ...
# 
# this will export your definition as 
# 
# <name>_386BSD 
# 
# The export is done in two different ways: (1) in the form required 
# by make (ie. -D<name>_386BSD="<value>"), and (2) into a sedscript.  
# In a subsequent makefile the first form can be referenced by 
#
# $(<name>_386BSD) 
#
# and the second form, the sedscript (called var.sed), will be applied
# to a *.raw file to replace values in any type of script. This is 
# done under the target stamp-expand by the command:
# 
# sed -f var.sed < file.raw > file 
# 
# The macros replaced in the raw file are made up like this: 
# 
# $(<name>_386BSD) 
# 
# this notation protects you from misinterpreting a raw file macro as 
# a shell variable, since shell script raw files are where these macro
# expansions are most often used. Yet it is not restricted to shell
# scripts, you can use those macro forms in man files (which is strongly
# recommended under the FILES section) or configuration scripts too.
#
# But take care of typos: An unresolved macro name will *not* be
# detected, so the shell (or whatever) will complain at runtime or
# will do weird things.
# 
# IMPORTANT NOTICE
#
# Please put any decision that implies a certain system configuration 
# not likely to be shared by any other 386BSD user into this overall 
# Makefile and export the definition. We like to have this Makefile as 
# the only subject to change for an average user. (Of course we do not 
# care about SYSV vs. BSD flags here. :-)

## system library 
SYSLIB= LIBROOT TEXLIB MFLIB PSLIB DVILIB FONTLIB TEXINPUTS \
	TEXFORMATS BIBTEX MFINPUTS MFBASES TFM PK VF CMSRC

## user's private lib
USRLIB= LIBROOT_USER TEXLIB_USER MFLIB_USER PSLIB_USER DVILIB_USER \
	FONTLIB_USER TEXINPUTS_USER TEXFORMATS_USER BIBTEX_USER \
	MFINPUTS_USER MFBASES_USER TFM_USER PK_USER VF_USER

## the other definitions (which actually include the libs from above)
EXPORT= CC INSTALL PSOUT DEFDEV DEFRES RESRES GERMAN FORMATS BASES MANEXT \
	DEFAR MFSCRCOL MFSCRROW BINDIR MANDIR $(SYSLIB) $(USRLIB)

#
# Congratulations you are finshed with setup!
###########################################################################
# ... why are you still here? Go ahead and have your TeX system be made!
#

## the exported variables will be re-imported from Makefile.inc here
.if exists(Makefile.inc)
.include "Makefile.inc"
.endif

## Definitions local to this Makefile
### here we have the directory names of the packages to be made. It's
### important to have different names for the target and the directory
### otherwise make will end prematurely with '.. is up to date!'.
TeX=TeX3.14
Dvips=dvips
Xdvi=xdvi
dvidrv=beebe
dviutils=dvi2tty

ALL= TeX Dvips Xdvi dvidrv dviutils
ALLDIR= $(TeX) $(Dvips) $(Xdvi) $(dvidrv) $(dviutils)

# Build targets
all: Makefile.inc stamp-port $(ALL) $(INTERNATIONAL) #user-tree

# in fact this is notreally used by the current release, since I lost
# most of the originals during hacking. But for subsequent releases
# this may be a good method to leave installed packages untouched
ORIGINAL-FILES= $(TeX)/Makefile $(dvidrv)/Makefile $(dviutils)/Makefile \
	$(TeX)/tex/convert $(TeX)/mf/convert $(TeX)/Install_INPUTS \
	$(TeX)/site.h 

stamp-backup:
	for i in $(ORIGINAL-FILES) ;\
	do \
	  if [ ! -e $$i.orig ] ;\
	  then \
	    echo "mv $$i $$i.orig" ;\
	  fi ; \
	done
	touch stamp-backup

restore-originals:
	for i in $(ORIGINAL-FILES) ;\
	do \
	  if [ -e $$i.orig ] ;\
	  then \
	    echo "mv $$i.orig $$i" ;\
	  fi ; \
	done
	rm stamp-backup

stamp-port: stamp-backup
	cd ./scripts ; \
	cp ./$(TeX).Makefile ../$(TeX)/Makefile ; \
	cp ./$(dvidrv).Makefile ../$(dvidrv)/Makefile ; \
	cp ./$(dviutils).Makefile ../$(dviutils)/Makefile ; \
	cp ./tex.convert ../$(TeX)/tex/convert ; \
	cp ./mf.convert ../$(TeX)/mf/convert 
	rm -f $(TeX)/mf/MFwindow/x11.c 
	-ln $(TeX)/mf/MFwindow/x11-Xlib.c $(TeX)/mf/MFwindow/x11.c 
	touch stamp-port 
#
#	patch -d ../$(Dvips) -N -c < dvips.diff

stamp-expand: Makefile
	cd ./scripts ; \
	echo $(seddef) |sed -f mksedscript.sed > var.sed ; \
	sed -f ./var.sed ./Install_INPUTS.raw > ../$(TeX)/Install_INPUTS ; \
	sed -f ./var.sed ./site.h.raw > ../$(TeX)/site.h ; \
	sed -f ./var.sed ./dvidr.raw > ./dvidr ; \
	sed -f ./var.sed ./dvidr.1.raw > ./dvidr.1 ; \
	if [ ! -d ../usr ] ; then \
	  echo "making ../usr"; \
	  mkdir -p ../usr ; \
	fi ; \
	sed -f ./var.sed ./Xdefaults.raw > ../usr/.Xdefaults-`hostname` ; \
	sed -f ./var.sed ./environ.sh.raw > ../usr/texsetup.sh ; \
	sed -f ./var.sed ./environ.csh.raw > ../usr/texsetup.csh ; \
	sed -f ./var.sed ./modes.mf.raw > ./modes.mf ;
	touch stamp-expand

TeX: stamp-port stamp-expand TeXprog

######
# Target to make TeX, MF & close friends
#
TeXprog:
	cd $(TeX) ; \
	make $(makedef) all ; \
	make $(makedef) manpages
# These have to be put off until later as we haven't yet installed
# the inputs
#	make $(makedef) formats 
#	make $(makedef) bases ; \
#
######
# this is here because we sometimes want to bypass the default make
# all to debug a part of $(TeX), to be used
#
# > make PASS="<whatever you want to pass over>" TeXpass 
#
TeXpass:
	cd $(TeX) ; \
	make $(makedef) $(PASS)
#
######
# the targets for make in package
#
Dvips: 
	cd $(Dvips) ; \
	make $(makedef) all	

Xdvi: 
	cd $(Xdvi) ;\
	make $(makedef) all

dvidrv: stamp-port
	cd $(dvidrv) ;\
	make $(makedef) $(BEEBEDRV)

dviutils: stamp-port
	cd $(dviutils) ;\
	make $(makedef) all
#
######
# Installation targets
#

stamp-inputs: stamp-tree
	cd $(TeX) ; \
	sh Install_INPUTS
	touch $(.TARGET)

install: stamp-inputs $(ALL:S/^/install./g)
	$(INSTALL) -m 755 scripts/dvidr $(BINDIR)
	$(INSTALL) -m 644 scripts/dvidr.1 $(MANDIR)/man$(MANEXT)
	-ln $(BINDIR)/dvidr $(BINDIR)/dvi
	-ln $(BINDIR)/dvidr $(BINDIR)/dvipr
	-ln $(BINDIR)/dvidr $(BINDIR)/mkfonts
#	cp -R usr $(LIBROOT)

install.TeX: stamp-tree
	cd $(TeX) ; \
	make $(makedef) install ; \
	make $(makedef) formats ; \
	make $(makedef) install-formats ; \
	make $(makedef) bases ; \
	make $(makedef) install-bases ; \
	make $(makedef) install-manpages ; \

install.Dvips: stamp-tree
	cd $(Dvips) ; \
	make $(makedef) install

install.Xdvi:
	cd $(Xdvi) ;\
	make $(makedef) install ;\
	make $(makedef) install.man

install.dvidrv:
	cd $(dvidrv) ;\
	make -k $(makedef) install
#does this
#	$(INSTALL) -m 755 scripts/mftopk $(BINDIR)
#	$(INSTALL) -m 644 scripts/mftopk.1 $(MANDIR)/man$(MANEXT)

install.dviutils:
	cd $(dviutils) ;\
	make $(makedef) install
#
######
# Cleanup targets
#
clean: $(ALL:S/^/clean./g)
	rm -f *~ \#*\# a.out *.o stamp-*
	cd ./scripts ; rm -f var.sed modes.mf dvidr.1 dvidr
	rm -r -f ./usr/*

clean.TeX: 
	cd $(TeX) ; \
	make $(makedef) clean ; \

clean.Dvips: 
	cd $(Dvips) ; \
	make $(makedef) clean

clean.Xdvi:
	cd $(Xdvi) ;\
	make $(makedef) clean ;\

clean.dvidrv:
	cd $(dvidrv) ;\
	make $(makedef) clean ;\

clean.dviutils:
	cd $(dviutils) ;\
	make $(makedef) clean ;\
#
######
# Very-cleanup targets
#
veryclean: $(ALL:S/^/veryclean./g)
	rm -f stamp-* Makefile.inc *~ #*# a.out *.o 
	cd ./scripts ; rm -f var.sed modes.mf dvidr.1 dvidr
	rm -r -f ./usr/*
	cd $(TeX) ; rm -f ./Makefile ./site.h ./Install_INPUTS \
			./mf/convert ./tex/convert 
	rm -f ./$(dvidrv)/Makefile ./$(dviutils)/Makefile

veryclean.TeX:
	cd $(TeX) ; \
	make $(makedef) veryclean ; \

veryclean.Dvips:
	cd $(Dvips) ; \
	make $(makedef) veryclean

veryclean.Xdvi:
	cd $(Xdvi) ;\
	make $(makedef) clean ;\

veryclean.dvidrv:
	cd $(dvidrv) ;\
	make $(makedef) veryclean ;\

veryclean.dviutils:
	cd $(dviutils) ;\
	make $(makedef) veryclean ;\
#
######
# Library trees
#
stamp-tree:
	for i in $(syslib) ; \
	do \
	  if [ ! -d $$i ] ; \
	  then \
	  echo "making $$i"; \
	    mkdir -p $$i ; \
	  fi ; \
	done
	chmod 1777 $(PK)
	touch $(.TARGET)

user-tree:
	for i in $(usrlib:S/\${HOME}/.\/usr/g) ; \
	do \
	  if [ ! -d $$i ] ; \
	  then \
	    echo "making $$i" ; \
	    mkdir -p $$i ; \
	  fi ; \
	done
#
######
#Internationalization of formats
#
.if $(GERMAN)
INTERNATIONAL=international

##tex defaults to english mode, use \germanTeX to switch to german.
INTERNATIONALIZE=\\input german.initex

international:
	cd $(TeX)/tex; \
	./initex plain.tex $(INTERNATIONALIZE) \\dump ; \
	./initex lplain.tex $(INTERNATIONALIZE) \\dump

.else
INTERNATIONAL=
.endif
#
######

###########################################################################
# target for the include file
#
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
#
###########################################################################



###################################################################The End.
