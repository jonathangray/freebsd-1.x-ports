# @(#)Makefile	1.14 18/9/92 (UKC)

# Top level makefile for ups

# You may want to edit some of the following variables.

# Where to install the X11 binary
X11BIN = /usr/X386/bin

# Where to put the manual page
MANDIR = /usr/X386/man/man1

# The suffix to use for the manual page
MANSUFFIX = 1

# The directory where X include files live.  The default is
# /usr/include.  This flag is ignored unless other than in the
# assignment to X11WNFLAGS below which is commented out by default.
X11INCLUDE=/usr/X386/include

# Extra flags to pass to the build of the Wn library.  Uncomment
# this if your X header files live in a non-standard place and
# you have set X11INCLUDE above.
X11WNFLAGS=MORE_CFLAGS=-I${X11INCLUDE}

# The arguments to cc needed to link against the Xlib library.
# Change this to an explicit path (e.g. /usr/X11/lib/libX11.a)
# if your Xlib lives in a place that won't be found by -lX11.
X11LIB = "-L/usr/X386/lib -lX11"

# If you want you can pass make variable assignments to the
# subdirectory makefiles by uncommenting and editing the
# following assugnment to SUBMAKEFLAGS.
#
# The example shows how to build ups with the -g flag (by
# default neither -g or -O is specified).
#
# You can put multiple space separated assignments here if you want.
#
SUBMAKEFLAGS = "MORE_CFLAGS=-O -g -DOS_BSDI -DARCH_BSDI386"

# Flags for RISC/os from Conor Doherty and Hal R. Brand
# Edit HERE below to the absolute path to the ups source directory
#SUBMAKEFLAGS ="MORE_CFLAGS=-Driscos -DNO_DIRENT -DNO_PUTENV -IHERE/misc/riscos"
#X11LIB=/usr/X11/Rw321/lib/libX11.a	# for MIPS RISCwindows interface

# If you are building ups for a Sun (any architecture) running SunOS 4.0.3
# or earlier then you must uncomment the following line.  If you don't
# you will get incorrect values for char and short parameters.  Note that
# this means you can't share a single ups binary between SunOS 4.1.1 and
# an earlier release.  You need to turn this flag on for SunOS 3.X.
#
#SUBMAKEFLAGS = "ARCH_CFLAGS=-DFIX_SHORT_PARAMS"

MAKE = make	# for makes that don't do this automatically

BINDIRS = ups
LIBDIRS = lib/libx11wn lib/libmtrprog lib/libukcprog \
	  lib/libarg lib/libMen3 lib/libobj

COMMONLIBS = libmtrprog libukcprog libarg libMen3 libobj 
X11LIBS = libx11wn ${COMMONLIBS}
SUBDIRS = ${LIBDIRS} ${BINDIRS}

# Note: we use MFLAGS for the recursive makes rather than MAKEFLAGS.
# This is because the BDSi make loops forever if you say `make -',
# which is what you get if you say `${MAKE} -${MAKEFLAGS} when MAKEFLAGS
# is not set to anything.

x11ups: ups/develhdrs ${X11LIBS}
	cd ups && ${MAKE} ${MFLAGS} X11LIB=${X11LIB} ${SUBMAKEFLAGS}

libx11wn:
	cd lib/$@ && ${MAKE} ${MFLAGS} ${SUBMAKEFLAGS} ${X11WNFLAGS}

${COMMONLIBS}:
	cd lib/$@ && ${MAKE} ${MFLAGS} ${SUBMAKEFLAGS}

install:
	cd ups && ${MAKE} ${MFLAGS} ${SUBMAKEFLAGS} \
			X11BIN=${X11BIN} \
			MANDIR=${MANDIR} MANSUFFIX=${MANSUFFIX} \
			checkxinstall

# Try to leave the source directory as it came in the distribution
clean:
	for i in ${SUBDIRS}; do \
		(cd $$i && ${MAKE} ${MFLAGS} clean); \
		rm -rf $$i/develhdrs; \
	done

# The ups source assumes that various UKC libraries and header files
# are installed.  We distribute ups so that the source is self contained:
# this target makes the symlinks needed to support this.
ups/develhdrs:
	@echo Making links ...
	@-for i in ${SUBDIRS}; do mkdir $$i/develhdrs; done
	@-for i in Men3 arg mtrprog obj x11wn; do \
		mkdir lib/lib$$i/develhdrs/local; \
	done
	@-for i in Men3 obj; do \
		ln -s ../../../libx11wn/wn.h lib/lib$$i/develhdrs/local/wn.h; \
	done
	@-for i in arg mtrprog obj x11wn; do \
		ln -s ../../../libukcprog/ukcprog.h \
					lib/lib$$i/develhdrs/local/ukcprog.h; \
	done
	@-for i in arg obj; do \
		ln -s ../../libmtrprog lib/lib$$i/develhdrs/mtrprog; \
	done
	@-ln -s ../../lib/libmtrprog ups/develhdrs/mtrprog
	@-mkdir ups/develhdrs/local
	@-ln -s ../../../lib/libarg/arg.h ups/develhdrs/local/arg.h
	@-ln -s ../../../lib/libMen3/menu3.h ups/develhdrs/local/menu3.h
	@-ln -s ../../../lib/libobj ups/develhdrs/local/obj
	@-ln -s ../../../lib/libukcprog/ukcprog.h ups/develhdrs/local/ukcprog.h
	@-ln -s ../../../lib/libx11wn/wn.h ups/develhdrs/local/wn.h

# The rest of this makefile is concerned only with making releases from
# the master source directory.

# Odd files needed for particular architecures.
MISCFILES = misc/riscos/stdlib.h

FILES = Makefile README README.multiarch imake.sh imakefiles.shar ${MISCFILES}
FILELIST = releasefiles
CHECKRELEASE = checkrelease
RELROOT = releases
RELNAME = ups-`sccs prs -d:I: ups/sccsdata.c`

# Make a release as a compressed tar file
release: ${CHECKRELEASE} filelist ${RELROOT}
	@echo Creating release file ${RELNAME} ...
	@sed -e "s;^;${RELNAME}/;" -e 's;/./;/;' < ${FILELIST} > thesefiles
	@rm -f ${RELNAME}
	@ln -s . ${RELNAME}
	@tar cf - `cat thesefiles` | compress -c > ${RELROOT}/${RELNAME}.tar.Z
	@chmod 444 ${RELROOT}/${RELNAME}.tar.Z
	@rm -f ${RELNAME}

testrelease:
	${MAKE} ${MFLAGS} CHECKRELEASE= RELNAME=ups-test release

${RELROOT}:
	mkdir ${RELROOT}

# Check that everything is checked in OK for a release
checkrelease:
	@echo "Checking release status ..."
	@for i in . ${SUBDIRS}; do \
		(if cd $$i && make checksccs; then \
			echo $$i OK for release; \
		 else \
			echo $$i not ready for release; \
			exit 1; \
		 fi); \
	done

checksccs:
	sccsinfo ${FILES}

# Put into ${FILELIST} a list of all the files that make up an
# ups source release.
filelist:
	@: > ${FILELIST}
	@for i in . ${SUBDIRS}; do \
		echo Adding files from $$i; \
		(cd $$i && make list) | sed "s;^;$$i/;" >> ${FILELIST}; \
	done

# List one to a line the files in this directory that should go in a release
list:
	@echo CHANGES ${FILES} | tr ' ' '\012'
