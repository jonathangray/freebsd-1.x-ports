# Makefile for SCM (Scheme implementation intended for JACAL).
# Copyright (C) 1990, 1991, 1992, 1993 Aubrey Jaffer.
# See the file "COPYING" for terms applying to this program

# directory where COPYING and Init.scm reside.
IMPLPATH=/usr/local/lib/scm/
#this one is good while debugging
#IMPLPATH=`pwd`/

# Pathname where Init.scm resides.  This directory must also contain COPYING.
IMPLINIT=$(IMPLPATH)Init.scm
# If pathname where Init.scm resides is not known in advance then
# SCM_INIT_PATH is the environment variable whose value is the
# pathname where Init.scm resides.
# IMPLINIT=

# directory where `make install' will put executable.
DEST=/usr/local/bin/
# directory where `make install' will put manual page.
MANDEST=/usr/local/man/man1/

#CC = your compiler
# -DRTL if this is a run-time library only (no interactive top level)
# -Dunix is required for SCO
# -DSVR4 is required for Solaris 2.x and other SVR4 clones
# -static is required with gcc under OSF/1 V1.2 on DEC Alpha.
# -DRECKLESS if you want most scm error checking disabled.
# -DSTACK_LIMIT=(HEAP_SEG_SIZE/2) to enable stack checking.
# -O if you want the optimizing C compiler to be used.
# -g for dynamic linking (symbols).
#CFLAGS=+O1		#workaround for HP-UX 9.01 bug.
#CFLAGS=-Wl,-E		#dynamic link flags for HP-UX 9.01.
#CFLAGS=-O
CFLAGS=-DSICP
#CC = cc -hvector1 -hscalar1		#for UNICOS,
CC = gcc -g -O2 -Wall
#CC = gcc -O2 -traditional
LD = ld

#FFLAGS are flags for inexact numbers (floating point)
# -DFLOATS turns on support for inexact numbers.
# -DENGNOT for engineering notation instead of scientific notation.

FFLAGS=-DFLOATS

# append any names of user extension files
# -lansi for SCO and XENIX
# -lm for -DFLOATS
# -lcurses for `curses'; some systems also require -ltermcap
# -lncurses on linux (curses has bugs).
# ELIBS=../regex-0.11/regex.o
LIBS=$(ELIBS) -lm

# RGXFLAGS=-I../regex-0.11/

# INITS= the initialization calls for user extension files.
#File:	INITS line:		functions defined:
#sc2.c	init_sc2\(\)	substring-move-left!, substring-move-right!,
#			substring-fill!, append!, and last-pair
#rgx.c	init_rgx\(\)	regcomp and regexec.
#crs.c	init_crs\(\)	lots.

INITS=-DINITS=\;

#seds to help find names not unique in first 8 characters (name8s)
# for BSD nm format
SED_TO_STRIP_NM=sed -e '/.*\.o$$/d' -e 's/.* _//' -e 's/.* T //'
#old, bad for T [^_] on suns: SED_TO_STRIP_NM=sed -e '/.*\.o$$/d' -e 's/.* _//'
# For a System V nm where plain C identifiers have _ prepended:
#SED_TO_STRIP_NM=sed -e '/^_[A-Za-z][A-za-z0-9_]*[ 	|].*|extern|!d' -e 's/|/ /g' -e 's/^_//'
# For a System V nm where plain C identifiers have nothing prepended:
#SED_TO_STRIP_NM=sed -e '/^[A-Za-z][A-za-z0-9_]*[ 	|].*|extern|!d' -e 's/|/ /g'

#you should not need to change below this line.

SHELL = /bin/sh
DFLAG = -DIMPLINIT=\"$(IMPLINIT)\"
ffiles = time.o repl.o fscl.o sys.o feval.o subr.o funif.o #sc2.o #rgx.o
efiles = time.o repl.o escl.o sys.o eeval.o subr.o eunif.o #sc2.o #rgx.o
cfiles = scm.c time.c repl.c scl.c sys.c eval.c subr.c sc2.c unif.c \
	rgx.c crs.c dynl.c
confiles = INSTALL.unix configure configure.in scmconfig.h.in \
	mkinstalldirs Makefile.in acconfig-1.5.h
hfiles = scm.h config.h setjump.h patchlvl.h
ifiles = Init.scm Transcen.scm Link.scm
tfiles = test.scm example.scm pi.scm pi.c split.scm
dfiles = ANNOUNCE README COPYING scm.1 scm.doc QUICKREF MANUAL ChangeLog \
	code.doc proto.h
mfiles = makefile.unix makefile.msc makefile.bor makefile.tur\
	makefile.djg makefile.emx makefile.qc compile.amiga link.amiga \
	makefile.aztec makefile.ast makefile.prj dmakefile makefile.wcc
vfiles = setjump.mar VMSBUILD.COM VMSGCC.COM
afiles = $(dfiles) $(cfiles) $(hfiles) $(ifiles) $(tfiles) $(mfiles) $(vfiles)

all:	scm
scheme:	scm

# -DINITS= the initialization calls for user extension files.
dbscm:	escm.a ../wb/db.a scm.c scm.h config.h patchlvl.h makefile.unix
	$(CC) -o dbscm $(ELIBS) \
	$(CFLAGS) $(INITS)init_db\(\)\; \
	scm.c escm.a ../wb/db.a
	rm escm.a
curscm:	escm.a rgx.o crs.o scm.c
	$(CC) -o curscm $(ELIBS) \
	$(CFLAGS) $(INITS)init_crs\(\)\;init_rgx\(\) \
	scm.c escm.a rgx.o crs.o -lncurses
	rm escm.a
escm.a:	$(efiles) makefile.unix
	ar crv escm.a $(efiles)

dscm:	$(ffiles) dscm.o dynl.o
	$(CC) -o dscm $(ffiles) dscm.o $(LIBS) dynl.o -ldld -static # for Linux
shlscm:	$(ffiles) dscm.o dynl.o		 # -WlE for HP-UX
	$(CC) -Wl,-E -o dscm $(ffiles) dscm.o $(LIBS) dynl.o -ldld
dscm.o:	scm.c scm.h config.h patchlvl.h
	$(CC) $(CFLAGS) -c $(FFLAGS) -DINITS=init_dynl\(\) scm.c
	mv scm.o dscm.o

scm:	$(ffiles) fscm.o # dynl.o
	$(CC) -o scm $(ffiles) fscm.o $(LIBS) # dynl.o -ldld
fscm.o:	scm.c scm.h config.h patchlvl.h
	$(CC) $(CFLAGS) -c $(FFLAGS) $(INITS) scm.c # -DINITS=init_dynl\(\)
	mv scm.o fscm.o
#From: fred@sce.carleton.ca (Fred J Kaudel)
#-U, if get can't include machine file errors from sys/signal.h during
#cc [-Ac] compiles under HP-UX A.08.07, or similar, then try changing to
#either cc -Aa, if available, for this file only, or replace the above $(CC)
#line with the following:
#	$(CC) $(CFLAGS) -c $(FFLAGS) $(INITS) scm.c -U__hp9000s800

fscl.o:	scl.c scm.h config.h
	$(CC) $(CFLAGS) -c $(FFLAGS) scl.c
	mv scl.o fscl.o
feval.o:	eval.c scm.h config.h
	$(CC) $(CFLAGS) -c $(FFLAGS) eval.c
	mv eval.o feval.o
funif.o:	unif.c scm.h
	$(CC) $(CFLAGS) -c $(FFLAGS) unif.c
	mv unif.o funif.o

escm:	$(efiles) escm.o # dynl.o
	$(CC) -o escm $(efiles) escm.o $(ELIBS) # dynl.o -ldld
escm.o:	scm.c scm.h config.h patchlvl.h
	$(CC) $(CFLAGS) -c $(INITS) scm.c # -DINITS=init_dynl\(\)
	mv scm.o escm.o
escl.o:	scl.c scm.h config.h
	$(CC) $(CFLAGS) -c scl.c
	mv scl.o escl.o
eeval.o:	eval.c scm.h config.h
	$(CC) $(CFLAGS) -c eval.c
	mv eval.o eeval.o
eunif.o:	unif.c scm.h
	$(CC) $(CFLAGS) -c unif.c
	mv unif.o eunif.o

repl.o:	repl.c scm.h config.h setjump.h
	$(CC) $(CFLAGS) -c $(DFLAG) repl.c
sys.o:	sys.c scm.h config.h setjump.h
	$(CC) $(CFLAGS) -c sys.c
time.o:	time.c scm.h config.h
	$(CC) $(CFLAGS) -c time.c
subr.o:	subr.c scm.h config.h
	$(CC) $(CFLAGS) -c subr.c
dynl.o:	dynl.c scm.h config.h
	$(CC) $(CFLAGS) -DDLD -c dynl.c	# -DRTL

# Now for dynamically loadable files:

sc2.o:	sc2.c scm.h
	$(CC) $(CFLAGS) -c sc2.c
rgx.o:	rgx.c scm.h config.h
	$(CC) $(CFLAGS) $(RGXFLAGS) -c rgx.c
crs.o:	crs.c scm.h config.h
	$(CC) $(CFLAGS) -c crs.c
crs:	crs.o
	$(LD) -r -o crs crs.o -lncurses #/usr/lib/libncurses.a -nostdlib

shlib:			# for HP-UX
	$(CC) $(CFLAGS) -c +z sc2.c
	$(LD) -b sc2.o -o sc2

turtlegr.o:	turtlegr.c scm.h config.h makefile.unix
	$(CC) -c $(CFLAGS) $(FFLAGS) -DX11 -I/usr/local/X11R5/include \
	turtlegr.c

both:	scm escm

proto.h:	$(cfiles)
	rm -f proto.h
	mkproto $(cfiles) > proto.h

libscm.a:	rtlscm.o $(ffiles)
	rm -f libscm.a
	ar rc libscm.a rtlscm.o $(ffiles)
	ranlib libscm.a

rtlscm.o:	scm.c scm.h config.h patchlvl.h
	$(CC) $(CFLAGS) -c $(FFLAGS) -DRTL $(INITS)init_user_scm\(\) scm.c
	mv scm.o rtlscm.o

scm.doc:	scm.1
	nroff -man scm.1 >scm.doc

install:	scm scm.1
	-mkdir -p $(DEST)
	-cp scm $(DEST)
	-strip $(DEST)scm
	-mkdir -p $(MANDEST)
	-cp scm.1 $(MANDEST)
	-mkdir -p $(IMPLPATH)
	-cp Init.scm $(IMPLPATH)
	-cp Transcen.scm $(IMPLPATH)
	-cp COPYING $(IMPLPATH)
	-mkdir -p /usr/local/lib
	-cp -r slib /usr/local/lib

tar:	scm.tar
scm.tar:	temp/scm
	cd temp; tar cohf ../scm.tar scm
	chmod 664 scm.tar
shar:	scm.shar
scm.shar:	temp/scm
	cd temp; shar scm >../scm.shar
	chmod 664 scm.shar
dclshar:	scm.com
com:	scm.com
scm.com:	temp/scm
	cd temp; dclshar scm >../scm.com
	chmod 664 scm.com
zip:	scm.zip
scm.zip:	temp/scm
	cd temp; zip -r ../scm.zip scm
	chmod 664 scm.zip
pubzip:	temp/scm
	cd temp; zip -ru ../../pub/scm.zip scm
	chmod 664 ../pub/scm.zip

temp/scm:	$(afiles)
	-rm -rf temp
	mkdir temp
	mkdir temp/scm
	ln  $(afiles) temp/scm

dist:	scm.tar.gz
	mv -f scm.tar.*z ../dist/
tar.Z:	scm.tar.Z
scm.tar.Z:	scm.tar
	-rm -f scm.tar.Z
	compress scm.tar
	chmod 664 scm.tar.Z
tar.gz:	scm.tar.gz
scm.tar.gz:	scm.tar
	-rm -f scm.tar.*z
	gzip scm.tar
	chmod 664 scm.tar.*z
shar.Z:	scm.shar.Z
scm.shar.Z:	scm.shar
	-rm -f scm.shar.Z
	compress scm.shar
	chmod 664 scm.shar.Z

temp/scmconfig:	$(confiles)
	-rm -rf temp/scmconfig
	-mkdir temp
	mkdir temp/scmconfig
	ln  $(confiles) temp/scmconfig
confdist:	scmconfig.tar.gz
scmconfig.tar.gz:	temp/scmconfig
	cd temp; tar cohf ../scmconfig.tar scmconfig
	chmod 664 scmconfig.tar
	-rm -f scmconfig.tar.*z
	gzip scmconfig.tar
	chmod 664 scmconfig.tar.*z

pubdiffs:	temp/scm
	mv temp/scm temp/nscm
	cd temp;unzip ../../pub/scm.zip
	rm temp/scm/proto.h
	-rm -f scm.diffs
	-diff -c temp/scm temp/nscm > scm.diffs
	-rm -rf temp
	ls -l scm.diffs

distdiffs:	temp/scm
	mv temp/scm temp/nscm
	cd temp;zcat ../../dist/scm4*.tar.gz | tar xvf -
	rm temp/scm/proto.h
	-rm -f scm.pat
	-diff -c temp/scm temp/nscm > scm.pat
	-rm -rf temp
	ls -l scm.pat

checks: check echeck
check:	scm test.scm
	scm -ftest.scm -e'(test-sc4)(test-cont)(test-inexact)(gc)'
echeck:	escm test.scm
	scm -ftest.scm -e'(test-sc4)(test-cont)(gc)'

lint:	lints
lints:	$(cfiles) $(hfiles)
	lint $(CFLAGS) $(FFLAGS) $(cfiles) | tee lints
#	lint $(CFLAGS) $(cfiles) | tee lintes
name8:	name8s
name8s:	scm
	nm scm |\
	$(SED_TO_STRIP_NM) |\
	sort -u|\
	awk '{	if (substr(l,1,8)==substr($$1,1,8)) {\
			if (p) print l;\
			print $$1;p=0;stat=1\
		}else p=1;\
		l=$$1\
	     }END{exit stat}' -
ctags:	$(hfiles) $(cfiles) code.doc
	etags $(hfiles) $(cfiles) code.doc
TAGS:
tags:	$(hfiles) $(cfiles) $(ifiles) $(mfiles) $(vfiles) \
	MANUAL code.doc README
	etags $(hfiles) $(cfiles) $(ifiles) $(mfiles) $(vfiles) \
	MANUAL code.doc README
mostlyclean:
clean:
	-rm -f *~ \#* *\# *.orig *.rej a.out core lints tmp*
	-rm -rf temp hobtemp
distclean:
	-rm -f *~ \#* *.o *\# *.orig *.rej a.out core lints tmp*
	-rm -rf temp hobtemp
realclean:
	-rm -f *~ \#* *.o *\# *.orig *.rej a.out core TAGS lints tmp* scm.doc
	-rm -rf temp hobtemp
realempty:	temp/scm
	-rm -f $(afiles)

pkg: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST scm
