#%#### makefile AUTOMATICALLY GENERATED ######
#%############### DO NOT EDIT ################
#%### EDIT makefile.src AND make config ######
DEPTH = ../
CONFIG = ../config/freebsd
HERE = cnv/
# FreeBSD 1.0 EPSILON configuration file.
# With X11
# Most common conversion programs compiled.

ExtraCFLAGS = 

ROFF = nroff
ROFFOPT = -man
ROFFPIPE = | lpr

OTHERJUNK = core *~ #*#

PBMDIR = ../../pbmplus
INCPBMPLUS = -I$(PBMDIR) -I$(PBMDIR)/ppm -I$(PBMDIR)/pgm -I$(PBMDIR)/pbm
LIBPBMPLUS = $(PBMDIR)/ppm/libppm.a $(PBMDIR)/pgm/libpgm.a \
		$(PBMDIR)/pbm/libpbm.a 

INCTIFF = -I../../pbmplus/libtiff
LIBTIFF = -L../../pbmplus/libtiff -ltiff

LIBX11 = -L/usr/X386/lib -lX11
INCX11 = -I/usr/X386/include

RI = ../include
RL = ../lib
DIRS =
ALLDIRS =
PGMS =

ExtraLIBS = 
LIBS = $(RL)/librle.a $(DIRLIBS)  ${ExtraLIBS}

MachFLAGS = 
CFLAGS = -O $(DFLAGS) $(DIRFLAGS) $(ExtraCFLAGS) $(IFLAGS) $(MachFLAGS) 
IFLAGS = -I. -I$(RI)

SHELL = /bin/sh

# Description file for rle conversion programs.


#set	DEST	CNV_DEST	Where to install the tools
DEST = /usr/local/bin

# Executables.  The .out will be stripped off in the install action.
#ifadd	ABEKASA60	PGMS		rletoabA60.out
#ifadd	ABEKASA62	DIRS		rletoabA62
#ifadd	CUBICOMP	PGMS		cubitorle.out
#ifadd	DVIRLE		DIRS		tex
#ifadd	GIF		DIRS		rletogif
#ifadd	GIF		PGMS		giftorle.out
#ifadd	GRAYFILES	PGMS		graytorle.out
#ifadd	GRAYFILES	PGMS		rletogray.out
#ifadd	MACPAINT	PGMS		painttorle.out
#ifadd	MACPAINT	PGMS		rletopaint.out
#ifadd	PBMPLUS		PGMS		pgmtorle.out ppmtorle.out rletoppm.out
#ifadd	POSTSCRIPT	PGMS		rletops.out
#ifadd	TARGA		PGMS		targatorle.out
#ifadd	TIFF		PGMS		rletotiff.out tifftorle.out
#ifadd  SUNRASTER	PGMS		rastorle.out
#ifadd  SUNRASTER	PGMS		rletorast.out
#ifadd  WASATCH		PGMS		wasatchrle.out
#ifadd	WAVEFRONT	PGMS		rlatorle.out rletorla.out
#ifnadd	NO_TOOLS	PGMS		rawtorle.out rletoraw.out
#ifnadd	NO_TOOLS	PGMS		rletoascii.out

#emit PGMS
PGMS =  giftorle.out graytorle.out rletogray.out painttorle.out rletopaint.out pgmtorle.out ppmtorle.out rletoppm.out rletops.out targatorle.out rletotiff.out tifftorle.out rawtorle.out rletoraw.out rletoascii.out

#emit DIRS
DIRS =  rletogif

# ALLDIRS is all subdirectories, regardless of whether they are active
ALLDIRS = rletoabA62 rletogif tex

# Default is what is to be made if nothing is specified.
default: $(PGMS) subdirs

# Everything depends on the rle header and libraries.
# keep_make_happy keeps make happy if $PGMS is empty
$(PGMS) keep_make_happy: $(RI)/rle.h $(RI)/rle_config.h

# Programs that need specific make rules (presumably all of them)

#ifdef ABEKASA60
# rletoabA60 - RLE to Abekas A60.
# Will build with the default rule.
#endif ABEKASA60

#ifdef CUBICOMP
# cubitorle - Cubicomp to RLE.
# Will build with the default rule.
#endif CUBICOMP

#ifdef GRAYFILES
# graytorle - Combine gray scale images into RLE file.
# rletogray - Split RLE file into gray scale images.
# Will build with the default rule.
#endif GRAYFILES

#ifdef MACPAINT
# painttorle - MacPaint to RLE.
# rletopaint - RLE to MacPaint.
# Will build with the default rule.
#endif MACPAINT

#ifdef PBMPLUS
# pgmtorle - pgm format to RLE
# ppmtorle - ppm format to RLE
# rletoppm - RLE to ppm format
pgmtorle.out: pgmtorle.c
	$(CC) $(CFLAGS) $(INCPBMPLUS) $*.c $(LIBS) $(LIBPBMPLUS) -o $*.new
	mv $*.new $@
ppmtorle.out: ppmtorle.c
	$(CC) $(CFLAGS) $(INCPBMPLUS) $*.c $(LIBS) $(LIBPBMPLUS) -o $*.new
	mv $*.new $@
rletoppm.out: rletoppm.c
	$(CC) $(CFLAGS) $(INCPBMPLUS) $*.c $(LIBS) $(LIBPBMPLUS) -o $*.new
	mv $*.new $@
#endif

#ifdef POSTSCRIPT
# rletops - RLE to postscript.
# Will build with the default rule.
#endif POSTSCRIPT

#ifdef SUNRASTER
# rastorle - Sun raster to RLE.
#@rastorle.out: rastorle.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBSUNRASTER) -lm -o $*.new
#@	mv $*.new $@
#@rletorast.out: rletorast.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBSUNRASTER) -lm -o $*.new
#@	mv $*.new $@
#endif

#ifdef TARGA
# targatorle - AT&T Targa to RLE
# Will build with the default rule.
#endif TARGA

#ifdef TIFF
# tifftorle - Convert TIFF images to RLE
# rletotiff - Convert RLE images to TIFF
rletotiff.out: rletotiff.c
	$(CC) $(CFLAGS) $(INCTIFF) $*.c $(LIBS) $(LIBTIFF) -o $*.new
	mv $*.new $@
tifftorle.out: tifftorle.c
	$(CC) $(CFLAGS) $(INCTIFF) $*.c $(LIBS) $(LIBTIFF) -o $*.new
	mv $*.new $@
#endif

#ifdef WASATCH
# wasatchrle - Wasatch Paintbox to RLE.
# Will build with the default rule.
#endif WASATCH

#ifdef WAVEFRONT
# rlatorle - Wavefront RLA to RLE
# rletorla - RLE to Wavefront RLA
# Will build with the default rule.
#endif WAVEFRONT

# Install stuff
install: default install-pgm install-subdirs

# Clean up binaries.
clean: clean-pgm

# Clean up installed binaries
pristine: pristine-pgm

# Default rule for making a program from its .c file.
# The mv will not be done if there is an error, so a broken program
# will not be left around.
.SUFFIXES:
.SUFFIXES: .out .c
.c.out:
	$(CC) $(CFLAGS) $*.c $(LIBS) -lm -o $*.new
	mv $*.new $@

# Dependency lines.  Make sure to #ifdef them.
# DO NOT DELETE THIS LINE

#ifdef CUBICOMP
#@cubitorle.o: ${RI}/rle.h
#endif CUBICOMP
#ifdef MACPAINT
painttorle.o: ${RI}/rle.h
rletopaint.o: ${RI}/rle.h
#endif MACPAINT
#ifdef SUNRASTER
#@rastorle.o: ${RI}/rle.h
#@rletorast.o: ${RI}/rle.h
#endif SUNRASTER
#ifdef ABEKASA60
#@rletoabA60.o: ${RI}/rle.h
#endif ABEKASA60
#ifdef POSTSCRIPT
rletops.o: ${RI}/rle.h
#endif POSTSCRIPT
#ifdef TARGA
targatorle.o: ${RI}/rle.h
#endif TARGA
#ifdef WASATCH
#@wasatchrle.o: ${RI}/rle.h
#endif WASATCH
#ifdef GRAYFILES
graytorle.o: ${RI}/rle.h
rletogray.o: ${RI}/rle.h
#endif GRAYFILES
################ Beginning of makefile.tlr ################
# Stuff that goes at the end of all the makefiles, but is not
# configuration parameters should be in this file.  It is included
# after the makefile.src.

subdirs:
	@sh -c "if test 'x$(DIRS)' != x ; then eval \
		'set -e ; for dir in $(DIRS) ; do \
			(cd \$$dir ; echo Make ${HERE}\$$dir ; \
			 	make $(MFLAGS) $(DIRMFLAGS) ) ; \
		done' ; \
	  else \
		true ; \
	  fi"

# Install all programs
install-pgm: $(PGMS)
	@sh -c "if test 'x$?' != x ; then eval \
		'for pgm in $? ; do \
			dpgm=\`basename \$$pgm .out\` ; \
			echo cp \$$pgm $(DEST)/\$$dpgm ; \
			cp \$$pgm $(DEST)/\$$dpgm; \
		done' ; \
	else \
	    true ; \
	fi"
	touch install-pgm

# Install subdirectories
install-subdirs: subdirs
	@sh -c "if test 'x$(DIRS)' != x  ; then eval \
		'for dir in $(DIRS) ; do \
			(cd \$$dir ; echo Install ${HERE}\$$dir ; \
			 make $(MFLAGS) $(DIRMFLAGS) install) ; \
		done' ; \
	  else \
		true ; \
	  fi"

#config reads the configuration file and munches the apropriate make files
config: doit
	${DEPTH}Configure "${CONFIG}" "${DEPTH}" "${HERE}" "${MFLAGS}"

# post-config is for any actions a directory may require following
# configuration or reconfiguration.
post-config: doit

config-subdirs: doit
	@-if test 'x$(ALLDIRS)' != x ; then eval \
	    'for d in $(ALLDIRS) ; do \
		    ( cd $$d ; \
		      echo Configuring ${HERE}$$d ; \
		      ${DEPTH}Configure "${CONFIG}" "${DEPTH}" "${HERE}$$d/" \
				    "${MFLAGS}" ) \
	    done' ; \
	else \
	    true ; \
	fi

doit:


# Clean up installed stuff and binaries
# pristine-pgm target invoked pristine target in dirs that make programs
# pristine target in library directory is different.
# 
pristine-pgm: clean-pgm
	@-if test 'x$(PGMS)' != x ; then eval \
		'for pgm in $(PGMS); do \
			dpgm=`basename $$pgm .out`; \
			echo rm -f $(DEST)/$$dpgm; \
			rm -f $(DEST)/$$dpgm; \
		done' ; \
	else \
	    true ; \
	fi
	@-if test 'x$(ALLDIRS)' != x ; then eval \
	    'for dir in $(ALLDIRS); do \
		if test -d $$dir ; then \
			(cd $$dir; echo Make ${HERE}$$dir pristine ; \
			 make $(MFLAGS) pristine); \
		else \
			true; \
		fi; \
	    done' ; \
	else \
	    true ; \
	fi
	-rm -f install install-pgm

# Clean up binaries on program directories.
clean-pgm:
	-rm -f $(PGMS) install*
	-rm -f *.o *.out *.new
	-rm -f $(OTHERJUNK)
	@-if test 'x$(ALLDIRS)' != x ; then eval \
	    'for dir in $(ALLDIRS); do \
		if test -d $$dir ; then \
			(cd $$dir; echo Clean ${HERE}$$dir ; \
			 make $(MFLAGS) clean); \
		else \
			true; \
		fi; \
	    done' ; \
	else \
	    true ; \
	fi

#ifndef NO_MAKE_MAKEFILE
# Copy config action to here so that GNU make program will work.
#@makefile:: makefile.src
#@	${DEPTH}Configure "${CONFIG}" "${DEPTH}" "${HERE}" "${MFLAGS}"
#endif

# Keep people who like to 'make depend' happy.  Allow depend::
# actions in specific makefiles if desired.
depend::

################ End of makefile.tlr ################
