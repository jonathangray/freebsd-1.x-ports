# Generated automatically from Makefile.in by configure.
#
# Makefile for octave
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = .

srcdir = .
VPATH = .

include Makeconf

DISTFILES = COPYING ChangeLog Makefile.in Makeconf.in NEWS README \
	README.NLP THANKS INSTALL INSTALL.OCTAVE BUGS PLOTTING \
	PROJECTS f2c-compat.sh flibs.sh configure configure.in \
	config.guess octave.sh octave-mode.el doinstall.sh mkpath.c

# Complete directory trees to distribute.
DISTDIRS = dld

# Subdirectories to run make in for the primary targets.
SUBDIRS = libcruft liboctave  info src doc scripts 

# Subdirectories to run `make dist' in
DISTSUBDIRS = $(SUBDIRS) test

BINDISTFILES = COPYING ChangeLog NEWS README THANKS INSTALL.OCTAVE \
	BUGS PLOTTING PROJECTS octave.sh octave-mode.el doinstall.sh \
	mkpath

# Subdirectories to run `make dist' in
BINDISTSUBDIRS = doc scripts

all: mkpath
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
.PHONY: all

mkpath:
	$(CC) $(ALL_CFLAGS) $(srcdir)/mkpath.c -o mkpath

libcruft:
	cd libcruft ; $(MAKE) all
.PHONY: libcruft

liboctave:
	cd liboctave ; $(MAKE) all
.PHONY: liboctave

dld:
	cd dld ; $(MAKE) all
.PHONY: dld

info:
	cd info ; $(MAKE) all
.PHONY: info

src:
	cd src ; $(MAKE) all
.PHONY: src

scripts:
	cd scripts ; $(MAKE) all
.PHONY: scripts

doc:
	cd doc ; $(MAKE) all
.PHONY: doc

check:
	cd test; $(MAKE) check
.PHONY: check

octave.info:
	cd doc ; $(MAKE) octave.info
.PHONY: octave.info

INSTALL.info:
	cd doc ; $(MAKE) ../INSTALL.OCTAVE
.PHONY: INSTALL.info

BUGS.info:
	cd doc ; $(MAKE) ../BUGS
.PHONY: BUGS.info

install:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
.PHONY: install

uninstall:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
.PHONY: uninstall

tags:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
.PHONY: tags

TAGS:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
.PHONY: TAGS

clean:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
.PHONY: clean

mostlyclean:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
.PHONY: mostlyclean

distclean: clean
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
	rm -f *.a Makefile config.status
.PHONY: distclean

realclean:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $@; cd ..; done
	rm -f *.a Makefile config.status
.PHONY: realclean

# Now that things are under RCS control, we need to do the recursive
# chmod so that the distributed files end up with reasonable modes.

local-dist: octave.info INSTALL.info BUGS.info
	echo octave-local-$(version) > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln $(DISTFILES) `cat .fname`
	for dir in $(DISTDIRS); do ln -s ../$$dir `cat .fname`; done
	for dir in $(DISTSUBDIRS); do mkdir `cat .fname`/$$dir; cd $$dir; $(MAKE) $@; cd ..; done
	tar chf `cat .fname`.tar `cat .fname`
	rm -rf `cat .fname`
	tar xf `cat .fname`.tar
	chmod -R a+rwX `cat .fname`
	tar cf `cat .fname`.tar `cat .fname`
.PHONY: local-dist

links-for-dist: octave.info INSTALL.info BUGS.info
	echo octave-$(version) > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln $(DISTFILES) `cat .fname`
	for dir in $(DISTDIRS); do ln -s ../$$dir `cat .fname`; done
	for dir in $(DISTSUBDIRS); do mkdir `cat .fname`/$$dir; cd $$dir; $(MAKE) dist; cd ..; done
.PHONY: links-for-dist

links-for-bin-dist: octave.info INSTALL.info BUGS.info
	echo octave-$(version) > .fname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln $(BINDISTFILES) `cat .fname`
	for dir in $(BINDISTSUBDIRS); do mkdir `cat .fname`/$$dir; cd $$dir; $(MAKE) dist; cd ..; done
.PHONY: links-for-dist

binary-dist: links-for-bin-dist .fname
	strip src/octave
	ln src/octave `cat .fname`/octave
	tar chf `cat .fname`.tar `cat .fname`
	rm -rf `cat .fname`
	tar xf `cat .fname`.tar
	chmod -R a+rwX `cat .fname`
	tar cf `cat .fname`.tar `cat .fname`
	rm -rf `cat .fname` .fname
.PHONY: binary-dist

dist: links-for-dist
	tar chf `cat .fname`.tar `cat .fname`
	rm -rf `cat .fname`
	tar xf `cat .fname`.tar
	chmod -R a+rwX `cat .fname`
	tar cf `cat .fname`.tar `cat .fname`
	rm -rf `cat .fname` .fname
.PHONY: dist

doc-dist:
	echo octave-$(version) > .fname
	echo octave-doc-$(version).tar > .tarfname
	rm -rf `cat .fname`
	mkdir `cat .fname`
	ln README.doc COPYING `cat .fname`
	mkdir `cat .fname`/doc; cd doc; $(MAKE) doc-dist
	tar chf `cat .tarfname` `cat .fname`
	rm -rf `cat .fname`
	tar xf `cat .tarfname`
	chmod -R a+rwX `cat .fname`
	tar cf `cat .tarfname` `cat .fname`
	rm -rf `cat .fname` .fname
.PHONY: doc-dist

dist-z:
	echo octave-$(version).tar > .fname
	gzip --best `cat .fname`
.PHONY: dist-z

split-dist:
	echo octave-$(version).tar.gz > .fname
	split -b 1350k `cat .fname` `cat .fname`-
	rm -f .fname
.PHONY: split-dist

split-local-dist:
	echo octave-local-$(version).tar.gz > .fname
	split -b 1350k `cat .fname` `cat .fname`-
	rm -f .fname
.PHONY: split-local-dist

newversion:
	awk '/static[ \t]+char[ \t]+version_string\[\] [ \t]*=/ { \
	  beg = index ($$5, ".") + 1; \
	  end = index ($$5, ";") - 2; \
	  len_major = beg - 3; \
	  len_minor = end - beg + 1; \
	  major = substr ($$5, 2, len_major); \
	  minor = substr ($$5, beg, len_minor); \
	  printf ("static char version_string[] = \"%d.%d\";\n", \
		  major, ++minor); \
	  next; \
	} { print $$0 }' src/version.h > src/version.h.new
	mv src/version.h.new src/version.h
.PHONY: newversion
