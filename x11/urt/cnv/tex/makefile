#%#### makefile AUTOMATICALLY GENERATED ######
#%############### DO NOT EDIT ################
#%### EDIT makefile.src AND make config ######
DEPTH = ../../
CONFIG = ../../config/freebsd
HERE = cnv/tex/
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

RI = ../../include
RL = ../../lib
DIRS =
ALLDIRS =
PGMS =

ExtraLIBS = 
LIBS = -L$(RL) -lrle $(DIRLIBS)  ${ExtraLIBS}

MachFLAGS = 
CFLAGS = -O $(DFLAGS) $(DIRFLAGS) $(ExtraCFLAGS) $(IFLAGS) $(MachFLAGS) 
IFLAGS = -I. -I$(RI)

SHELL = /bin/sh

# Makefile for dvi to RLE conversion program(s)

#set	DEST	TOOLS_DEST
DEST = /usr/local/bin

DIRLIBS =  $(LIBMCTEX)
DIRFLAGS = -I$(INCMCTEX)
DIRMFLAGS = CONFFILE=${CONFFILE}

# If the McTeX library directory is in this directory, make it first.
#ifadd	DVIRLE_LIBSUBDIR	DIRS	lib
#emit DIRS
DIRS = 

# Executable programs
PGMS= dvirle1.out dvirle2.out
SCRIPTS= dvirle.sh topcrop.sh 

SRCS=	dvirle1.c dvirle2.c

default: all

all:	subdirs $(PGMS)

# Clean up installed stuff and binaries
pristine: pristine-pgm
	for pgm in $(SCRIPTS); do rm -f $(DEST)/`basename $$pgm .sh`; done
	-rm -f install-scripts

# Clean up binaries
clean: clean-pgm

# Everything depends on the rle header and libraries.
# Also depend on the .h files in $(INCMCTEX) (This may break your make)
$(PGMS): $(RI)/rle.h $(RI)/rle_config.h $(INCMCTEX)/*.h

install: default install-pgm install-scripts
	touch install

install-scripts: $(SCRIPTS)
	for pgm in $?; do d=$(DEST)/`basename $$pgm .sh`; \
		cp $$pgm $$d; chmod +x $$d; done
	touch install-scripts

# Default rule for making a program from its .c file.
# The mv will not be done if there is an error, so a broken program
# will not be left around.
.SUFFIXES:
.SUFFIXES: .out .c
.c.out:
	$(CC) $(CFLAGS) $< $(LIBS) -lm -o $*.new
	mv $*.new $@
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
