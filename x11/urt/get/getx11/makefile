#%#### makefile AUTOMATICALLY GENERATED ######
#%############### DO NOT EDIT ################
#%### EDIT makefile.src AND make config ######
DEPTH = ../../
CONFIG = ../../config/freebsd
HERE = get/getx11/
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

RI = ../../include
RL = ../../lib
DIRS =
ALLDIRS =
PGMS =

ExtraLIBS = 
LIBS = $(RL)/librle.a $(DIRLIBS)  ${ExtraLIBS}

MachFLAGS = 
CFLAGS = -O $(DFLAGS) $(DIRFLAGS) $(ExtraCFLAGS) $(IFLAGS) $(MachFLAGS) 
IFLAGS = -I. -I$(RI)

SHELL = /bin/sh

# Makefile for get/getx11 directory

# Default thing to make is everything
default: all

#set	DEST	GET_DEST	Where to install it.
DEST = /usr/local/bin

#ifdef XLIBINT_H
#@XGHC_C = XGetHClrs.c
#@XGHC_O = XGetHClrs.o
#endif

# Point to X11 include directory, if it's in a strange place
DIRFLAGS = $(INCX11)

# -lX11 is X11 support library
DIRLIBS = $(LIBX11) -lm

# Executables.  The .out will be stripped off in the install action.

X_C_FILES =\
	getx11.c x11_stuff.c timer.c map_scan.c in_cmap.c XCopyImg.c \
	$(XGHC_C)

X_O_FILES =\
	getx11.o x11_stuff.o timer.o map_scan.o in_cmap.o XCopyImg.o \
	$(XGHC_O)

PGMS = getx11.out

all: $(PGMS)

$(X_O_FILES): $(RI)/rle_config.h

# getx11 - read RLE file to X11 display
getx11.out: $(X_O_FILES) 
	${CC} $(CFLAGS) $(X_O_FILES) $(LIBS) -o getx11.new
	mv getx11.new getx11.out

# Incremental install, copies executable to DEST dir.
install: default install-pgm
	touch install

# Clean up binaries
clean:	clean-pgm

# Clean up installed code
pristine: pristine-pgm
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
