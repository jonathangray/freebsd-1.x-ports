#%#### makefile AUTOMATICALLY GENERATED ######
#%############### DO NOT EDIT ################
#%### EDIT makefile.src AND make config ######
DEPTH = ../
CONFIG = ../config/freebsd
HERE = get/
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

# Description file for rle get programs.

#set DEST GET_DEST	Where to install tools
DEST = /usr/local/bin

# Executables.  The .out will be stripped off in the install action.
#ifadd	BOBCAT		PGMS		getbob.out
#ifadd	BOBCAT		PGMS		read98721.out
#ifadd	FB		PGMS		getfb.out
#ifadd	IRIS		PGMS		getiris.out getmex.out
#ifadd	IRIS4D		PGMS		get4d.out
#ifadd	ORION		PGMS		get_orion.out
#ifadd	SUNTOOLS	PGMS		getsun.out
#ifadd	TAAC1		PGMS		gettaac.out
#ifadd	X10		PGMS		getx10.out
#ifadd  APOLLO		PGMS		getap.out
#ifadd  BOBCAT		PGMS		getren.out
#ifadd	CX3D		DIRS		cx3d
#ifadd	GRINNELL	DIRS		gmr27
#ifadd	HPQCR		DIRS		qcr
#ifadd	X11		DIRS		getx11

#emit PGMS
PGMS = 

#emit DIRS
DIRS =  getx11

# ALLDIRS is all subdirectories, regardless of whether they are active
ALLDIRS = gmr27 cx3d getx11 qcr

# Default is what is to be made if nothing is specified.
default: $(PGMS) subdirs

# To install
install: default install-pgm install-subdirs
	touch install

# Everything depends on the rle headers and libraries.
# 'keep_make_happy' is there to keep make happy if PGMS is empty.
$(PGMS) keep_make_happy: $(RI)/rle.h $(RI)/rle_config.h

# Programs that need specific make rules (presumably all of them)

#ifdef FB
# getfb - get to a BRL libfb display
#@getfb.out: getfb.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBFB) -lm -o $*.new
#@	mv $*.new $@
#endif

#ifdef XWINDOWS
# getx10 - get to an X display
#@getx10.out: getx10.c
#@	$(CC) $(CFLAGS) $(INCX10) $*.c $(LIBS) $(LIBX10) -lm -o $*.new
#@	mv $*.new $@
#endif

#ifdef APOLLO
# getap - get to an Apollo display
# will build with the default rule
#endif APOLLO

#ifdef IRIS
# getmex - get to a mex window on an Iris 2000 or 3000
#@getmex.out: getmex.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBIRIS) -lm -o $*.new
#@	mv $*.new $@
# getiris - get to an Iris ( no mex )
#@getiris.out: getiris.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBIRIS) -lm -o $*.new
#@	mv $*.new $@
#endif

#ifdef IRIS4D
# get4d - get to a max window on an Iris/4D.
#@get4d.out: get4d.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBIRIS4D) -lm -o $*.new
#@	mv $*.new $@
#endif

#ifdef BOBCAT
# getbob - get to an HP Bobcat (series hp9000/300)
#@getbob.out: getbob.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBBOBCAT) -lm -o $*.new
#@	mv $*.new $@
#@getren.out: getren.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBBOBCAT) -lm -o $*.new
#@	mv $*.new $@
#@read98721.out: read98721.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBBOBCAT) -lm -o $*.new
#@	mv $*.new $@
#endif

#ifdef SUNTOOLS
# getsun - display on Suns running SunTools
#@getsun.out: getsun.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBSUNTOOLS) -lm -o $*.new
#@	mv $*.new $@
#endif

#ifdef TAAC1
# gettaac - display on Sun TAAC 1
#@gettaac.out: gettaac.c
#@	$(CC) $(CFLAGS) $*.c $(LIBS) $(LIBTAAC1) -lm -o $*.new
#@	mv $*.new $@
#endif

# Default rule for making a program from its .c file.
# The mv will not be done if there is an error, so a broken program
# will not be left around.
.SUFFIXES:
.SUFFIXES: .out .c
.c.out:
	$(CC) $(CFLAGS) $< $(LIBS) -lm -o $*.new
	mv $*.new $@

# Clean up directory and installed binaries
pristine: pristine-pgm

# Just clean  up directory
clean: clean-pgm

# Dependency lines.  Make sure to #ifdef them.
# Not generally needed, as all programs depend on rle.h above.
# Except perhaps for special make rules, .o dependencies are not needed,
# as most rules go directly from .c to .out

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
