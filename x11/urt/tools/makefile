#%#### makefile AUTOMATICALLY GENERATED ######
#%############### DO NOT EDIT ################
#%### EDIT makefile.src AND make config ######
DEPTH = ../
CONFIG = ../config/freebsd
HERE = tools/
# FreeBSD configuration file.
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

INCTIFF = -I../../tiff
LIBTIFF = -L../../tiff -ltiff

LIBX11 = -L/usr/X386/lib -lX11
INCX11 = -I/usr/X386/include

RI = ../include
RL = ../lib
DIRS =
ALLDIRS =
PGMS =

ExtraLIBS = 
LIBS = -L$(RL) -lrle $(DIRLIBS)  ${ExtraLIBS}

MachFLAGS = 
CFLAGS = -O $(DFLAGS) $(DIRFLAGS) $(ExtraCFLAGS) $(IFLAGS) $(MachFLAGS) 
IFLAGS = -I. -I$(RI)

SHELL = /bin/sh

# Description file for rle tool programs. 

#set	DEST	TOOLS_DEST	Where to install the tools
DEST = /usr/local/bin

# Executables.  The .out will be stripped off in the install action.
PGMS = 	applymap.out \
	avg4.out \
	crop.out \
	fant.out \
	into.out \
	mcut.out \
	mergechan.out \
	pyrmask.out \
	repos.out \
	rleClock.out \
	rleaddcom.out \
	rlebg.out \
	rlebox.out \
	rlecat.out \
	rlecomp.out \
	rledither.out \
	rleflip.out \
	rlehdr.out \
	rlehisto.out \
	rleldmap.out \
	rlemandl.out \
	rlenoise.out \
	rlepatch.out \
	rleprint.out \
	rlequant.out \
	rlescale.out \
	rleselect.out \
	rlesetbg.out \
	rlespiff.out \
	rlesplice.out \
	rlesplit.out \
	rlestereo.out \
	rleswap.out \
	rlezoom.out \
	smush.out \
	to8.out \
	tobw.out \
	unexp.out \
	unslice.out

ALLDIRS = clock

# Default is what is to be made if nothing is specified.
default: $(PGMS)

# Everything depends on the rle headers and libraries.
$(PGMS): $(RI)/rle.h $(RI)/rle_config.h

# A couple programs also depend on the getraw definitions.
applymap.out rlebg.out: $(RI)/rle_raw.h

# Pyrmask comes in pieces, must be built special.

pyrlib.o: pyrlib.c $(RI)/pyramid.h $(RI)/rle.h $(RI)/rle_config.h
	$(CC) $(CFLAGS) pyrlib.c -c
pyrmask.out: pyrlib.o pyrmask.c $(RI)/pyramid.h
	$(CC) $(CFLAGS) -I$(RI) pyrmask.c pyrlib.o $(LIBS) -lm -o pyrmask.new
	mv pyrmask.new pyrmask.out

# rlebox and crop use some common code.
rle_box.o: $(RI)/rle.h $(RI)/rle_config.h $(RI)/rle_raw.h

crop.out: crop.c rle_box.o
	${CC} ${CFLAGS} crop.c rle_box.o ${LIBS} -o crop.new
	mv crop.new crop.out
rlebox.out: rlebox.c rle_box.o
	${CC} ${CFLAGS} rlebox.c rle_box.o ${LIBS} -o rlebox.new
	mv rlebox.new rlebox.out

# rleClock has it's own directory, must be built special

rleClock.out: clock/font.c clock/font.h clock/font.src clock/rleClock.c
	(cd clock ; make)

# Incremental install, copies everything ("$?") since last install to DEST dir.
install: $(PGMS) install-pgm
	touch install

# Clean up installed stuff and binaries
pristine: pristine-pgm

# Clean up binaries.
clean:	clean-pgm

# Default rule for making a program from its .c file.
# The mv will not be done if there is an error, so a broken program
# will not be left around.
.SUFFIXES:
.SUFFIXES: .out .c .o
.c.out:
	$(CC) $(CFLAGS) $< $(LIBS) -lm -o $*.new
	mv $*.new $@

.c.o:
	$(CC) -c $(CFLAGS) $<
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
