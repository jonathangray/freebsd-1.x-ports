# Generated automatically from Makefile.in by configure.
# DIST: This is the distribution Makefile for Emacs.  configure can
# DIST: make most of the changes to this file you might want, so try
# DIST: that first.

# make all	to compile and build Emacs.
# make install	to install it.
# make TAGS	to update tags tables.
#
# make clean  or  make mostlyclean
#      Delete all files from the current directory that are normally
#      created by building the program.	 Don't delete the files that
#      record the configuration.  Also preserve files that could be made
#      by building, but normally aren't because the distribution comes
#      with them.
#
#      Delete `.dvi' files here if they are not part of the distribution.
# 
# make distclean
#      Delete all files from the current directory that are created by
#      configuring or building the program.  If you have unpacked the
#      source and built the program without creating any other files,
#      `make distclean' should leave only the files that were in the
#      distribution.
# 
# make realclean
#      Delete everything from the current directory that can be
#      reconstructed with this Makefile.  This typically includes
#      everything deleted by distclean, plus more: C source files
#      produced by Bison, tags tables, info files, and so on.
#
# make extraclean
#      Still more severe - delete backup and autosave files, too.

SHELL = /bin/sh
MAKE = make  # BSD doesn't have it as a default.

# ==================== Things `configure' Might Edit ====================

CC=gcc
CPP=gcc -E
C_SWITCH_SYSTEM= 
ALLOCA=
LN_S=ln -s
CFLAGS=-g -O 
C_SWITCH_X_SITE=-I/usr/X386/include
LD_SWITCH_X_SITE=-L/usr/X386/lib
YACC=yacc

### These help us choose version- and architecture-specific directories
### to install files in.

### This should be the number of the Emacs version we're building,
### like `18.59' or `19.0'.
version=19.19

### This should be the name of the configuration we're building Emacs
### for, like `mips-dec-ultrix' or `sparc-sun-sunos'.
configuration=i386--386bsd

### Libraries which should be edited into lib-src/Makefile.
libsrc_libs= 

# ==================== Where To Install Things ====================

# The default location for installation.  Everything is placed in
# subdirectories of this directory.  The default values for many of
# the variables below are expressed in terms of this one, so you may
# not need to change them.  This defaults to /usr/local.
prefix=/usr/gnu

# Like `prefix', but used for architecture-specific files.
exec_prefix=${prefix}

# Where to install Emacs and other binaries that people will want to
# run directly (like etags).
bindir=${exec_prefix}/bin

# Where to install architecture-independent data files.	 ${lispdir}
# and ${etcdir} are subdirectories of this.
datadir=${prefix}/lib

# Where to install and expect the files that Emacs modifies as it
# runs.	 These files are all architecture-independent. Right now, the
# only such data is the locking directory; ${lockdir} is a
# subdirectory of this.
statedir=${prefix}/lib

# Where to install and expect executable files to be run by Emacs
# rather than directly by users, and other architecture-dependent
# data.	 ${archlibdir} is a subdirectory of this.
libdir=${exec_prefix}/lib

# Where to install Emacs's man pages, and what extension they should have.
mandir=${prefix}/man/man1
manext=.1

# Where to install and expect the info files describing Emacs.	In the
# past, this defaulted to a subdirectory of ${prefix}/lib/emacs, but
# since there are now many packages documented with the texinfo
# system, it is inappropriate to imply that it is part of Emacs.
infodir=${prefix}/info

# Where to find the source code.  The source code for Emacs's C kernel is
# expected to be in ${srcdir}/src, and the source code for Emacs's
# utility programs is expected to be in ${srcdir}/lib-src.  This is
# set by the configure script's `--srcdir' option.
srcdir=/usr/ports/emacs

# ==================== Emacs-specific directories ====================

# These variables hold the values Emacs will actually use.  They are
# based on the values of the standard Make variables above.

# Where to install the lisp files distributed with
# Emacs.  This includes the Emacs version, so that the
# lisp files for different versions of Emacs will install
# themselves in separate directories.
lispdir=${datadir}/emacs/${version}/lisp

# Directories Emacs should search for lisp files specific
# to this site (i.e. customizations), before consulting
# ${lispdir}.  This should be a colon-separated list of
# directories.
locallisppath=${datadir}/emacs/site-lisp

# Where Emacs will search to find its lisp files.  Before
# changing this, check to see if your purpose wouldn't
# better be served by changing locallisppath.  This
# should be a colon-separated list of directories.
lisppath=${locallisppath}:${lispdir}

# Where Emacs will search for its lisp files while
# building.  This is only used during the process of
# compiling Emacs, to help Emacs find its lisp files
# before they've been installed in their final location.
# It's usually identical to lisppath, except that the
# entry for the directory containing the installed lisp
# files has been replaced with ../lisp.  This should be a
# colon-separated list of directories.
buildlisppath=${srcdir}/lisp

# Where to install the other architecture-independent
# data files distributed with Emacs (like the tutorial,
# the cookie recipes and the Zippy database). This path
# usually contains the Emacs version number, so the data
# files for multiple versions of Emacs may be installed
# at once.
etcdir=${datadir}/emacs/${version}/etc

# Where to create and expect the locking directory, where
# the Emacs locking code keeps track of which files are
# currently being edited.
lockdir=${statedir}/emacs/lock

# Where to put executables to be run by Emacs rather than
# the user.  This path usually includes the Emacs version
# and configuration name, so that multiple configurations
# for multiple versions of Emacs may be installed at
# once.
archlibdir=${libdir}/emacs/${version}/${configuration}

# ====================== Developer's configuration =======================

# The following assignments make sense if you're running Emacs on a single
# machine, one version at a time, and  you want changes to the lisp and etc
# directories in the source tree to show up immediately in your working
# environment.  It saves a great deal of disk space by not duplicating the
# lisp and etc directories.

#disabled# lispdir=${srcdir}/lisp
#disabled# externallispdir=${srcdir}/externallisp
#disabled# locallisppath=${srcdir}/site-lisp:${datadir}/emacs/site-lisp
#disabled# etcdir=${srcdir}/etc
#disabled# lockdir=${srcdir}/lock
#disabled# archlibdir=${srcdir}/lib-src
#disabled# infodir=${srcdir}/info

# ==================== Utility Programs for the Build ====================

# Allow the user to specify the install program.
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

# ============================= Targets ==============================

# Flags passed down to subdirectory makefiles.
MFLAGS = 

# Subdirectories to make recursively.  `lisp' is not included
# because the compiled lisp files are part of the distribution
# and you cannot remake them without installing Emacs first.
SUBDIR = lib-src src

# The makefiles of the directories in $SUBDIR.
SUBDIR_MAKEFILES = lib-src/Makefile src/Makefile oldXMenu/Makefile

# Subdirectories to install, and where they'll go.
# lib-src's makefile knows how to install it, so we don't do that here.
# When installing the info files, we need to do special things to
# avoid nuking an existing dir file, so we don't do that here;
# instead, we have written out explicit code in the `install' targets.
COPYDIR = ${srcdir}/etc ${srcdir}/lisp
COPYDESTS = ${etcdir} ${lispdir}

all:	src/paths.h ${SUBDIR}

removenullpaths=sed -e 's/^://' -e 's/:$$//' -e 's/::/:/'

# We force the rebuilding of src/paths.h because the user might give
# make different values for the various directories.  Since we use
# move-if-change, src/paths.h only actually changes if the user did
# something notable, so the only unnecessary work we do is in building
# src/paths.h.tmp, which isn't much.
# Note that sed is not in /bin on 386bsd.
src/paths.h: Makefile ${srcdir}/src/paths.h.in FRC.src.paths.h
	@echo "Producing \`src/paths.h' from \`src/paths.h.in'."
	@(lisppath=`echo ${lisppath} | ${removenullpaths}` ;		\
	  buildlisppath=`echo ${buildlisppath} | ${removenullpaths}` ;	\
	  sed < ${srcdir}/src/paths.h.in > src/paths.h.tmp		\
	  -e 's;\(#.*PATH_LOADSEARCH\).*$$;\1 "'$${lisppath}'";'	\
	  -e 's;\(#.*PATH_DUMPLOADSEARCH\).*$$;\1 "'$${buildlisppath}'";' \
	  -e 's;\(#.*PATH_EXEC\).*$$;\1 "${archlibdir}";'		\
	  -e 's;\(#.*PATH_INFO\).*$$;\1 "${infodir}";'			\
	  -e 's;\(#.*PATH_DATA\).*$$;\1 "${etcdir}";'			\
	  -e 's;\(#.*PATH_LOCK\).*$$;\1 "${lockdir}/";')
	@${srcdir}/move-if-change src/paths.h.tmp src/paths.h

src:	lib-src FRC.src
lib-src: FRC.lib-src

.RECURSIVE: ${SUBDIR}

${SUBDIR}: ${SUBDIR_MAKEFILES} FRC
	cd $@; $(MAKE) all ${MFLAGS} \
		CC='${CC}' CFLAGS='${CFLAGS}' MAKE='${MAKE}' \
		prefix='${prefix}' srcdir='${srcdir}/$@' libdir='${libdir}'

## We build the makefiles for the subdirectories here so that we can
## edit the values for the path variables into them.  This means that
## when the user has built them from this makefile once, they will use
## the right default values for the path variables.
lib-src/Makefile: ${srcdir}/lib-src/Makefile.in Makefile
	rm -f lib-src/Makefile.tmp
	@echo "Producing \`lib-src/Makefile' from \`${srcdir}/lib-src/Makefile.in'."
	@(echo "# This file is generated from \`${srcdir}/lib-src/Makefile.in'." ; \
	  echo "# If you are thinking about editing it, you should seriously consider" ; \
	  echo "# running \`make lib-src/Makefile' at the top of the" ;	\
	  echo "# Emacs build tree instead, or editing" ;		\
	  echo "# \`${srcdir}/lib-src/Makefile.in' itself." ;		\
	  sed < ${srcdir}/lib-src/Makefile.in				\
	  -e 's|^\(version *=\).*$$|\1'"${version}"'|'			\
	  -e 's|^\(configname *=\).*$$|\1'"${configuration}"'|'		\
	  -e 's|^\(prefix *=\).*$$|\1'"${prefix}"'|'			\
	  -e 's|^\(exec_prefix *=\).*$$|\1'"${exec_prefix}"'|'		\
	  -e 's|^\(libdir *=\).*$$|\1'"${libdir}"'|'			\
	  -e 's|^\(srcdir *=\).*$$|\1'"${srcdir}"'/lib-src|'		\
	  -e 's|^\(VPATH *=\).*$$|\1'"${srcdir}"'/lib-src|'		\
	  -e 's|^\(archlibdir *=\).*$$|\1'"${archlibdir}"'|'		\
	  -e 's|^\(ALLOCA *=\).*$$|\1'"${ALLOCA}"'|'			\
	  -e 's|^\(YACC *=\).*$$|\1'"${YACC}"'|'			\
	  -e 's|^CC *=.*$$|CC='"${CC}"'|'				\
	  -e 's|^CFLAGS *=.*$$|CFLAGS='"${CFLAGS}"'|' \
	  -e 's|^C_SWITCH_SYSTEM *=.*$$|C_SWITCH_SYSTEM='"${C_SWITCH_SYSTEM}"'|' \
	  -e 's|^LOADLIBES *=.*$$|LOADLIBES='"${libsrc_libs}"'|'	\
	  -e '/^# DIST: /d') > lib-src/Makefile.tmp
	@${srcdir}/move-if-change lib-src/Makefile.tmp lib-src/Makefile
	chmod -w lib-src/Makefile

src/Makefile: ${srcdir}/src/Makefile.in Makefile
	rm -f src/Makefile.tmp
	@echo "Producing \`src/Makefile' from \`${srcdir}/src/Makefile.in'."
	@(echo "# This file is generated from \`${srcdir}/src/Makefile.in'." ; \
	  echo "# If you are thinking about editing it, you should seriously consider" ; \
	  echo "# running \`make src/Makefile' at the top of the" ;	\
	  echo "# Emacs build tree instead, or editing" ;		\
	  echo "# \`${srcdir}/src/Makefile.in' itself." ;		\
	  sed < ${srcdir}/src/Makefile.in				\
	  -e 's|^\(srcdir *=\).*$$|\1${srcdir}/src|'			\
	  -e 's|^\(VPATH *=\).*$$|\1${srcdir}/src|'			\
	  -e 's|^CC *=.*$$|CC=${CC}|'					\
	  -e 's|^CPP *=.*$$|CPP=${CPP}|'				\
	  -e 's|^LN_S *=.*$$|LN_S=${LN_S}|'				\
	  -e 's|^CFLAGS *=.*$$|CFLAGS=${CFLAGS}|' 			\
	  -e 's|^\(C_SWITCH_SYSTEM *=\).*$$|\1'"${C_SWITCH_SYSTEM}"'|'	\
	  -e 's|^\(LD_SWITCH_X_SITE *=\).*$$|\1${LD_SWITCH_X_SITE}|'	\
	  -e '/^# DIST: /d') > src/Makefile.tmp
	@${srcdir}/move-if-change src/Makefile.tmp src/Makefile
	chmod -w src/Makefile

oldXMenu/Makefile: ${srcdir}/oldXMenu/Makefile.in Makefile
	rm -f oldXMenu/Makefile.tmp
	@echo "Producing \`oldXMenu/Makefile' from \`${srcdir}/oldXMenu/Makefile.in'."
	@(echo "# This file is generated from \`${srcdir}/oldXMenu/Makefile.in'." ; \
	  echo "# If you are thinking about editing it, you should seriously consider" ; \
	  echo "# running \`make oldXMenu/Makefile' at the top of the" ; \
	  echo "# Emacs build tree instead, or editing" ;		\
	  echo "# \`${srcdir}/oldXMenu/Makefile.in' itself." ;		\
	  sed < ${srcdir}/oldXMenu/Makefile.in				\
	  -e 's|^\(srcdir *=\).*$$|\1'"${srcdir}"'/oldXMenu|'		\
	  -e 's|^\(VPATH *=\).*$$|\1'"${srcdir}"'/oldXMenu|'		\
	  -e 's|^\(C_SWITCH_X_SITE *=\).*$$|\1'"${C_SWITCH_X_SITE}"'|'	\
	  -e 's|^CC *=.*$$|CC='"${CC}"'|'				\
	  -e 's|^DEFS *=.*$$|DEFS='"${DEFS}"'|' \
	  -e '/^# DIST: /d') > oldXMenu/Makefile.tmp
	@${srcdir}/move-if-change oldXMenu/Makefile.tmp oldXMenu/Makefile
	chmod -w oldXMenu/Makefile

Makefile:
	./config.status

# ==================== Installation ====================

## If we let lib-src do its own installation, that means we
## don't have to duplicate the list of utilities to install in
## this Makefile as well.

## On AIX, use tar xBf.
## On Xenix, use tar xpf.

## We delete each directory in ${COPYDESTS} before we copy into it;
## that way, we can reinstall over directories that have been put in
## place with their files read-only (perhaps because they are checked
## into RCS).  In order to make this safe, we make sure that the
## source exists and is distinct from the destination.
install: all do-install

### Note that we copy the DOC-* files from the build etc directory
### as well as lots of things from ${srcdir}/etc.
do-install: mkdir
	(cd lib-src; \
	  $(MAKE) install ${MFLAGS} prefix=${prefix} \
	    exec_prefix=${exec_prefix} bindir=${bindir} libdir=${libdir} \
	    archlibdir=${archlibdir})
	-set ${COPYDESTS} ; \
	 for dir in ${COPYDIR} ; do \
	   if [ `(cd $$1 && pwd)` != `(cd $${dir} && pwd)` ] ; then \
	     rm -rf $$1 ; \
	   fi ; \
	   shift ; \
	 done
	-set ${COPYDESTS} ; \
	 mkdir ${COPYDESTS} ; \
	 for dir in ${COPYDIR} ; do \
	   dest=$$1 ; shift ; \
	   [ -d $${dir} ] \
	   && [ `(cd $${dir} && /bin/pwd)` != `(cd $${dest} && /bin/pwd)` ] \
	   && (echo "Copying $${dir}..." ; \
	       (cd $${dir}; tar -cf - . )|(cd $${dest};umask 0; tar -xvf - ); \
	       for subdir in `find $${dest} -type d ! -name RCS -print` ; do \
		 rm -rf $${subdir}/RCS ; \
		 rm -rf $${subdir}/CVS ; \
		 rm -f  $${subdir}/\#* ; \
		 rm -f  $${subdir}/*~ ; \
	       done) ; \
	 done
	if [ `(cd ./etc; /bin/pwd)` != `(cd ${srcdir}/etc; /bin/pwd)` ]; \
	then \
	   echo "Copying etc/DOC* ..." ; \
	   (cd etc; tar -cf - DOC*)|(cd ${etcdir}; umask 0; tar -xvf - ); \
	else true; fi
	if [ `(cd ${srcdir}/info && /bin/pwd)` != `(cd ${infodir} && /bin/pwd)` ]; \
	then (cd ${srcdir}/info ; \
	     if [ ! -f ${infodir}/dir ] && [ -f dir ]; then \
	       ${INSTALL_DATA} dir ${infodir}/dir ; \
	     fi ; \
	     for f in cl* emacs* forms* gnus* info* vip* ; do \
	       ${INSTALL_DATA} $$f ${infodir}/$$f ; \
	     done); \
	else true; fi
	cd ${srcdir}/etc; for page in emacs etags ctags ; do \
	  ${INSTALL_DATA} $${page}.1 ${mandir}/$${page}${manext} ; \
	done
	${INSTALL_PROGRAM} src/emacs ${bindir}/emacs-${version}
	chmod 1755  ${bindir}/emacs-${version}
	rm -f ${bindir}/emacs
	ln ${bindir}/emacs-${version} ${bindir}/emacs

### Build all the directories we're going to install Emacs in.	Since
### we may be creating several layers of directories (for example,
### /usr/local/lib/emacs/19.0/mips-dec-ultrix4.2), we use make-path
### instead of mkdir.  Not all systems' mkdirs have the `-p' flag.
mkdir: FRC.mkdir
	./lib-src/make-path ${COPYDESTS} ${lockdir} ${infodir} ${mandir} \
	  ${bindir} ${datadir} ${libdir} \
	  `echo ${locallisppath} | sed 's/:/ /'`
	chmod 777 ${COPYDESTS} ${lockdir}

### Delete all the installed files that the `install' target would
### create (but not the noninstalled files such as `make all' would
### create).
###
### Don't delete the lisp and etc directories if they're in the source tree.
uninstall:
	(cd lib-src; 					\
	 $(MAKE) ${MFLAGS} uninstall			\
	    prefix=${prefix} exec_prefix=${exec_prefix}	\
	    bindir=${bindir} libdir=${libdir} archlibdir=${archlibdir})
	for dir in ${lispdir} ${etcdir} ; do 		\
	  case `(cd $${dir} ; pwd)` in			\
	    `(cd ${srcdir} ; pwd)`* ) ;;		\
	    * ) rm -rf $${dir} ;;			\
	  esac ;					\
	  case $${dir} in				\
	    ${datadir}/emacs/${version}/* )		\
	      rm -rf ${datadir}/emacs/${version}	\
	    ;;						\
	  esac ;					\
	done
	(cd ${infodir}; rm -f cl* emacs* forms* info* vip*)
	(cd ${mandir};  rm -f emacs.1 etags.1 ctags.1)
	(cd ${bindir};  rm -f emacs-${version} emacs)


### Some makes seem to remember that they've built something called FRC,
### so you can only use a given FRC once per makefile.
FRC FRC.src.paths.h FRC.src FRC.lib-src FRC.mkdir:
FRC.mostlyclean FRC.clean FRC.distclean FRC.realclean:

# ==================== Cleaning up and miscellanea ====================

### `mostlyclean'
###      Like `clean', but may refrain from deleting a few files that people
###      normally don't want to recompile.  For example, the `mostlyclean'
###      target for GCC does not delete `libgcc.a', because recompiling it
###      is rarely necessary and takes a lot of time.
mostlyclean: FRC.mostlyclean
	(cd src;      make ${MFLAGS} mostlyclean)
	(cd oldXMenu; make ${MFLAGS} mostlyclean)
	(cd lib-src;  make ${MFLAGS} mostlyclean)
	(cd man;      make ${MFLAGS} mostlyclean)

### `clean'
###      Delete all files from the current directory that are normally
###      created by building the program.  Don't delete the files that
###      record the configuration.  Also preserve files that could be made
###      by building, but normally aren't because the distribution comes
###      with them.
### 
###      Delete `.dvi' files here if they are not part of the distribution.
clean: FRC.clean
	(cd src;      make ${MFLAGS} clean)
	(cd oldXMenu; make ${MFLAGS} clean)
	(cd lib-src;  make ${MFLAGS} clean)
	(cd man;      make ${MFLAGS} clean)

### `distclean'
###      Delete all files from the current directory that are created by
###      configuring or building the program.  If you have unpacked the
###      source and built the program without creating any other files,
###      `make distclean' should leave only the files that were in the
###      distribution.
top_distclean=\
	rm -f config.status build-install ; \
	rm -f Makefile ${SUBDIR_MAKEFILES} ; \
	(cd lock ; rm -f *)
distclean: FRC.distclean
	(cd src;      make ${MFLAGS} distclean)
	(cd oldXMenu; make ${MFLAGS} distclean)
	(cd lib-src;  make ${MFLAGS} distclean)
	(cd man;      make ${MFLAGS} distclean)
	${top_distclean}
	

### `realclean'
###      Delete everything from the current directory that can be
###      reconstructed with this Makefile.  This typically includes
###      everything deleted by distclean, plus more: C source files
###      produced by Bison, tags tables, info files, and so on.
### 
###      One exception, however: `make realclean' should not delete
###      `configure' even if `configure' can be remade using a rule in the
###      Makefile.  More generally, `make realclean' should not delete
###      anything that needs to exist in order to run `configure' and then
###      begin to build the program.
realclean: FRC.realclean
	(cd src;      make ${MFLAGS} realclean)
	(cd oldXMenu; make ${MFLAGS} realclean)
	(cd lib-src;  make ${MFLAGS} realclean)
	(cd man;      make ${MFLAGS} realclean)
	${top_distclean}

### This doesn't actually appear in the coding standards, but Karl
### says GCC supports it, and that's where the configuration part of
### the coding standards seem to come from.  It's like distclean, but
### it deletes backup and autosave files too.
extraclean:
	for i in ${SUBDIR}; do (cd $$i; $(MAKE) ${MFLAGS} extraclean); done
	-(cd lock; rm *)
	-rm config.status config-tmp-*
	-rm -f *~ \#*

### Unlocking and relocking.  The idea of these productions is to reduce
### hassles when installing an incremental tar of Emacs.  Do `make unlock'
### before unlocking the file to take the write locks off all sources so
### that tar xvof will overwrite them without fuss.  Then do `make relock'
### afterward so that VC mode will know which files should be checked in
### if you want to mung them.
###
### Note: it's no disaster if these productions miss a file or two; tar
### and VC will swiftly let you know if this happens, and it is easily
### corrected.
SOURCES = ChangeLog GETTING.GNU.SOFTWARE INSTALL Makefile.in PROBLEMS \
	README build-install.in configure make-dist move-if-change

unlock:
	chmod u+w $(SOURCES) cpp/*
	-(cd elisp; chmod u+w Makefile README *.texi)
	(cd etc; make unlock)
	(cd lib-src; make unlock)
	(cd lisp; make unlock)
	(cd lisp/term; chmod u+w README *.el)
	(cd man; chmod u+w *texi* ChangeLog split-man)
	(cd oldXMenu; chmod u+w *.[ch] Makefile README)
	(cd src; make unlock)

relock:
	chmod u-w $(SOURCES) cpp/*
	-(cd elisp; chmod u-w Makefile README *.texi)
	(cd etc; make relock)
	(cd lib-src; make relock)
	(cd lisp; make relock)
	(cd lisp/term; chmod u+w README *.el)
	(cd man; chmod u+w *texi* ChangeLog split-man)
	(cd oldXMenu; chmod u+w *.[ch] Makefile README)
	(cd src; make relock)

TAGS tags:	lib-src
	(cd ${srcdir}/src; \
	 ../lib-src/etags *.[ch] ../lisp/*.el ../lisp/term/*.el)

check:
	@echo "We don't have any tests for GNU Emacs yet."

dist:
	cd ${srcdir}; make-dist

info:
	(cd ${srcdir}/man; make ${MFLAGS} info)
dvi:
	(cd ${srcdir}/man; make ${MFLAGS} dvi)