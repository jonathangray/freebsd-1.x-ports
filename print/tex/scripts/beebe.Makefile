# Makefile for files in /tex/dvi using generic Unix cc
#
# Current target list:
#	all	clean	countlines	lint
#	00arit	dvialw	dvibit	dvican	dvie72	dvieps	dvigd	dviimp
#	dvijep	dvijet	dvil3p	dvil75	dvim72	dvimac	dvimpi	dvio72
#	dvioki	dviPGM	dviprx	dvitos	keytst	lptops	lw78	texidx
#	tosprx	uuencode	xxu
#
# [28-Oct-87] -- added uuencode target
# [21-Oct-87] -- added dvie72 and dvieps
# [22-Sep-87] -- added generic target dviPGM (say "make PGM=foo dvifoo" to
#		 build dvifoo); add XCFLAGS to allow extra compile-time
#		 options without having to respecify the long path variables
# [15-Apr-87] -- rebuilt dependency lists using "awk -f include.awk *.c"
# [08-Jul-87] -- added dvil3p
# [20-Jul-87] -- added xxu and dvi.xrf

# Default C compiler
CC = $(CC_386BSD)

# Extra compiler options
XCFLAGS =

# Set paths according to your local conventions
CFLAGS = -DFONTPATH='"$(PK_386BSD)/"' \
	 -DSUBPATH='"$(DVILIB_386BSD)/"' \
	 -DDO_NOT_USE_MAGTABLE \
	 $(XCFLAGS)

LINTFLAGS = -bchx

# Need math library to load
LFLAGS = -lm

MV = mv

RM = /bin/rm

# Source file extension
C = .c

# Object file extension
O = .o

# dvixxx.c files

DVIXXX =	dvialw.c dvibit.c dvican.c dvie72.c dvieps.c dvigd.c \
		dviimp.c dvijep.c dvijet.c dvil3p.c dvil75.c dvim72.c \
		dvimac.c dvimpi.c dvio72.c dvioki.c dviprx.c dvitos.c

OTHERS = 	00arit.c keytst.c lptops.c lw78.c texidx.c tosprx.c xxu.c

HFILES =        abortrun.h      actfact.h       alldone.h       bitmap.h \
		chargf.h        charpk.h        charpxl.h \
		clrbmap.h       clrrow.h        commands.h      dbgopen.h \
		dispchar.h      dumpchar.h      dvifile.h       dvihead.h \
		dviinit.h       dviterm.h       f20open.h \
		fatal.h         fillrect.h      findpost.h      fixpos.h \
		fontfile.h      fontsub.h       gblprocs.h      gblvars.h \
		gendefs.h       getbmap.h       getbytes.h      getfntdf.h \
		getpgtab.h      inch.h          initglob.h      keydef.h \
		loadchar.h      machdefs.h      main.h          movedown.h \
		moveover.h      moveto.h        nosignex.h      openfont.h \
		option.h        outrow.h        prtpage.h       prxbmap.h \
		readfont.h      readgf.h        readpk.h        readpost.h \
		readpxl.h       reldfont.h      rulepxl.h       setchar.h \
		setfntnm.h      setrule.h       signex.h        skgfspec.h \
		skipfont.h      skpkspec.h      special.h       strchr.h \
		strcm2.h        strid2.h        strrchr.h       tctos.h \
		typedefs.h      usage.h         warning.h

# Include files that include others

DVIHEAD = dvihead.h machdefs.h typedefs.h

MAIN = main.h commands.h gendefs.h gblprocs.h gblvars.h

# Targets (lw78 is excluded--most Unix sites have Transcript or devps
# which offer similar features)

ALL=    dvialw	dvibit	dvican	dvie72	dvieps	dvigd	dviimp	dvijep\
	dvijet	dvil3p	dvil75	dvim72	dvimac	dvimpi	dvio72	dvioki\
	dviprx	dvitos	keytst	lptops	texidx	tosprx


all: $(ALL)

none:

clean:
	$(RM) -f *.o
	$(RM) -f 00arit \
		dvialw dvibit dvican dvie72 dvieps dvigd \
		dviimp dvijep dvijet dvil75 dvil3p dvim72\
		dvimac dvimpi dvio72 dvioki dviprx dvitos\
		keytst lptops texidx tosprx
	$(RM) -f .~* .#*

veryclean: clean

install:
	$(INSTALL_386BSD) -m 644 doc/dvi.1L \
		$(MANDIR_386BSD)/man$(MANEXT_386BSD)/dvixxx.$(MANEXT_386BSD)
	$(INSTALL_386BSD) -m 755 -s $(ALL) $(BINDIR_386BSD)


# Prestrip counts are for code with comments retained, and #include files
# inserted.  Poststrip counts have blank lines, # lines, and comments
# deleted.
countlines:
	for f in $(DVIXXX) $(OTHERS) ;\
	do \
		echo -n $$f [prestrip] `$(CC) $(CFLAGS) -C -E $$f | tee foo1.$$f | wc`;\
		echo " [poststrip]" `$(CC) $(CFLAGS) -E $$f |\
			egrep -v '^[ 	]*$$|^\#' | tee foo2.$$f | wc`;\
	done

lint:
	lint $(LINTFLAGS) dvialw.c
	lint $(LINTFLAGS) dvibit.c
	lint $(LINTFLAGS) dvican.c
	lint $(LINTFLAGS) dvie72.c
	lint $(LINTFLAGS) dvieps.c
	lint $(LINTFLAGS) dvigd.c
	lint $(LINTFLAGS) dviimp.c
	lint $(LINTFLAGS) dvijep.c
	lint $(LINTFLAGS) dvijet.c
	lint $(LINTFLAGS) dvil3p.c
	lint $(LINTFLAGS) dvil75.c
	lint $(LINTFLAGS) dvim72.c
	lint $(LINTFLAGS) dvimac.c
	lint $(LINTFLAGS) dvimpi.c
	lint $(LINTFLAGS) dvio72.c
	lint $(LINTFLAGS) dvioki.c
	lint $(LINTFLAGS) dviprx.c
	lint $(LINTFLAGS) dvitos.c
	lint $(LINTFLAGS) lptops.c
	lint $(LINTFLAGS) texidx.c
	lint $(LINTFLAGS) tosprx.c

# ======================================================================
# Dependency lists and rules

00arit:
		$(RM) -f 00arit$(O)
		$(RM) -f 00arit
		$(CC) -DSIZE=short $(CFLAGS) 00arit$(C) -o 00arit $(LFLAGS)
		00arit
		$(RM) -f 00arit$(O)
		$(RM) -f 00arit
		$(CC) -DSIZE=int   $(CFLAGS) 00arit$(C) -o 00arit $(LFLAGS)
		00arit
		$(RM) -f 00arit$(O)
		$(RM) -f 00arit
		$(CC) -DSIZE=long  $(CFLAGS) 00arit$(C) -o 00arit $(LFLAGS)
		00arit
		$(RM) -f 00arit$(O)
		$(RM) -f 00arit

dvialw:	dvialw$(O)
		$(CC) $(CFLAGS) dvialw$(O) -o dvialw $(LFLAGS)

dvibit:	dvibit$(O) keybrd$(O)
		$(CC) $(CFLAGS) dvibit$(O) keybrd$(O) -o dvibit $(LFLAGS)

dvican:	dvican$(O)
		$(CC) $(CFLAGS) dvican$(O) -o dvican $(LFLAGS)

dvie72:	dvie72$(O)
		$(CC) $(CFLAGS) dvie72$(O) -o dvie72 $(LFLAGS)

dvieps:	dvieps$(O)
		$(CC) $(CFLAGS) dvieps$(O) -o dvieps $(LFLAGS)

dvigd:	dvigd$(O)
		$(CC) $(CFLAGS) dvigd$(O) -o dvigd $(LFLAGS)

dviimp:	dviimp$(O)
		$(CC) $(CFLAGS) dviimp$(O) -o dviimp $(LFLAGS)

dvijep:	dvijep$(O)
		$(CC) $(CFLAGS) dvijep$(O) -o dvijep $(LFLAGS)

dvijet:	dvijet$(O)
		$(CC) $(CFLAGS) dvijet$(O) -o dvijet $(LFLAGS)

dvil3p:	dvil3p$(O)
		$(CC) $(CFLAGS) dvil3p$(O) -o dvil3p $(LFLAGS)

dvil75:	dvil75$(O)
		$(CC) $(CFLAGS) dvil75$(O) -o dvil75 $(LFLAGS)

dvim72:	dvim72$(O)
		$(CC) $(CFLAGS) dvim72$(O) -o dvim72 $(LFLAGS)

dvimac:	dvimac$(O)
		$(CC) $(CFLAGS) dvimac$(O) -o dvimac $(LFLAGS)

dvimpi:	dvimpi$(O)
		$(CC) $(CFLAGS) dvimpi$(O) -o dvimpi $(LFLAGS)

dvio72:	dvio72$(O)
		$(CC) $(CFLAGS) dvio72$(O) -o dvio72 $(LFLAGS)

dvioki:	dvioki$(O)
		$(CC) $(CFLAGS) dvioki$(O) -o dvioki $(LFLAGS)

dvi$(PGM):	dvi$(PGM)$(O) keybrd$(O)
		$(CC) $(CFLAGS) dvi$(PGM)$(O) keybrd$(O) -o dvi$(PGM) $(LFLAGS)

dviprx:	dviprx$(O)
		$(CC) $(CFLAGS) dviprx$(O) -o dviprx $(LFLAGS)

dvitos:	dvitos$(O)
		$(CC) $(CFLAGS) dvitos$(O) -o dvitos $(LFLAGS)

keytst:	keytst$(O) keybrd$(O)
		$(CC) $(CFLAGS) keytst$(O) keybrd$(O) -o keytst $(LFLAGS)

lptops:	lptops$(O)
		$(CC) $(CFLAGS) lptops$(O) -o lptops $(LFLAGS)

lw78:	lw78$(O)
		$(CC) $(CFLAGS) lw78$(O) -o lw78 $(LFLAGS)

texidx:	texidx$(O)
		$(CC) $(CFLAGS) texidx$(O) -o texidx $(LFLAGS)

tosprx:	tosprx$(O)
		$(CC) $(CFLAGS) tosprx$(O) -o tosprx $(LFLAGS)

uuencode:
	if [ ! -s story.dvi ] ;\
	then \
		tex story;\
	fi
	for f in alw can e72 eps gd imp jep jet l3p l75 m72 mac mpi o72 \
		oki prx tos ;\
	do \
		echo dvi$$f;\
		./dvi$$f -q <story.dvi >story.$$f 2>story.err-$$f;\
		if [ -s story.dvi-err ];\
		then \
			cat story.dvi-err >>story.err-$$f; \
		fi;\
		rm -f story.dvi-err;\
		if [ ! -s story.err-$$f ];\
		then \
			rm -f story.err-$$f;\
		fi;\
		uuencode story.$$f story.dvi-$$f >story.uue-$$f;\
		rm -f story.$$f;\
	done

xxu:	xxu$(O)
		$(CC) $(CFLAGS) xxu$(O) -o xxu $(LFLAGS)

dvi.xrf:	$(DVIXXX) $(HFILES)
	xref $(DVIXXX) >dvi.xrf

# Object file dependencies

dvialw$(O):	dvialw.c\
		$(DVIHEAD) $(MAIN) abortrun.h actfact.h alldone.h\
		chargf.h charpk.h charpxl.h clrrow.h dbgopen.h dvifile.h\
		dviinit.h dviterm.h f20open.h fatal.h findpost.h\
		fixpos.h fontfile.h fontsub.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h movedown.h moveover.h\
		moveto.h nosignex.h openfont.h option.h prtpage.h\
		readfont.h readgf.h readpk.h readpost.h readpxl.h\
		reldfont.h rulepxl.h setfntnm.h setrule.h signex.h\
		skgfspec.h skipfont.h skpkspec.h strchr.h strcm2.h\
		strid2.h strrchr.h tctos.h usage.h warning.h


dvibit$(O):	dvibit.c\
		$(DVIHEAD) keydef.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h\
		clrrow.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h findpost.h fixpos.h fontfile.h\
		fontsub.h getbytes.h getfntdf.h getpgtab.h inch.h\
		initglob.h movedown.h moveover.h moveto.h nosignex.h\
		openfont.h option.h prtpage.h readfont.h readgf.h\
		readpk.h readpost.h readpxl.h reldfont.h rulepxl.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvican$(O):	dvican.c\
		$(DVIHEAD) $(MAIN) abortrun.h actfact.h alldone.h\
		chargf.h charpk.h charpxl.h clrrow.h dbgopen.h dvifile.h\
		dviinit.h dviterm.h f20open.h fatal.h findpost.h\
		fixpos.h fontfile.h fontsub.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h movedown.h moveover.h\
		moveto.h nosignex.h openfont.h option.h prtpage.h\
		readfont.h readgf.h readpk.h readpost.h readpxl.h\
		reldfont.h rulepxl.h setfntnm.h setrule.h signex.h\
		skgfspec.h skipfont.h skpkspec.h special.h strchr.h\
		strcm2.h strid2.h strrchr.h tctos.h usage.h warning.h


dvie72$(O):	dvie72.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h initglob.h inch.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvieps$(O):	dvieps.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h initglob.h inch.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvigd$(O):	dvigd.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h initglob.h inch.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dviimp$(O):	dviimp.c\
		$(DVIHEAD) $(MAIN) abortrun.h actfact.h alldone.h\
		chargf.h charpk.h charpxl.h clrrow.h dbgopen.h dvifile.h\
		dviinit.h dviterm.h f20open.h fatal.h findpost.h\
		fixpos.h fontfile.h fontsub.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h movedown.h moveover.h\
		moveto.h nosignex.h openfont.h option.h prtpage.h\
		readfont.h readgf.h readpk.h readpost.h readpxl.h\
		reldfont.h rulepxl.h setfntnm.h setrule.h signex.h\
		skgfspec.h skipfont.h skpkspec.h special.h strchr.h\
		strcm2.h strid2.h strrchr.h tctos.h usage.h warning.h


dvijep$(O):	dvijep.c\
		$(DVIHEAD) $(MAIN) abortrun.h actfact.h alldone.h\
		chargf.h charpk.h charpxl.h clrrow.h dbgopen.h dvifile.h\
		dviinit.h dviterm.h f20open.h fatal.h findpost.h\
		fixpos.h fontfile.h fontsub.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h movedown.h moveover.h\
		moveto.h nosignex.h openfont.h option.h prtpage.h\
		readfont.h readgf.h readpk.h readpost.h readpxl.h\
		reldfont.h rulepxl.h setfntnm.h setrule.h signex.h\
		skgfspec.h skipfont.h skpkspec.h special.h strchr.h\
		strcm2.h strid2.h strrchr.h tctos.h usage.h warning.h


dvijet$(O):	dvijet.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dbgopen.h dvifile.h dviinit.h dviterm.h dispchar.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h initglob.h inch.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvil3p$(O):	dvil3p.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h

dvil75$(O):	dvil75.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvim72$(O):	dvim72.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvimac$(O):	dvimac.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvimpi$(O):	dvimpi.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvio72$(O):	dvio72.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvioki$(O):	dvioki.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


# Generic target (useful for debugging test versions)
dvi$(PGM)$(O):	dvi$(PGM).c $(HFILES)


dviprx$(O):	dviprx.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


dvitos$(O):	dvitos.c\
		$(DVIHEAD) bitmap.h $(MAIN) abortrun.h actfact.h\
		alldone.h chargf.h charpk.h charpxl.h clrbmap.h\
		clrrow.h dispchar.h dbgopen.h dvifile.h dviinit.h dviterm.h\
		f20open.h fatal.h fillrect.h findpost.h fixpos.h\
		fontfile.h fontsub.h getbmap.h getbytes.h getfntdf.h\
		getpgtab.h inch.h initglob.h loadchar.h movedown.h\
		moveover.h moveto.h nosignex.h openfont.h option.h\
		outrow.h prtpage.h readfont.h readgf.h readpk.h\
		readpost.h readpxl.h reldfont.h rulepxl.h setchar.h\
		setfntnm.h setrule.h signex.h skgfspec.h skipfont.h\
		skpkspec.h special.h strchr.h strcm2.h strid2.h\
		strrchr.h tctos.h usage.h warning.h


keybrd$(O):	keybrd.c\
		keydef.h


keytst$(O):	keytst.c\
		keydef.h


lptops$(O):	lptops.c\
		inch.h strcm2.h


tosprx$(O):	tosprx.c\
		machdefs.h typedefs.h gendefs.h gblprocs.h clrbmap.h
