# Generated automatically from Makefile.in by configure.
# Makefile for Texinfo distribution.	-*- Indented-Text -*-
# Copyright (C) 1993 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

#### Start of system configuration section. ####

srcdir = .
VPATH  = $(srcdir):$(common)

common = $(srcdir)/libtxi

CC = gcc

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

LN	= ln
RM	= rm -f
TAR	= tar
MKDIR	= mkdir

DEFS =  -DSTDC_HEADERS=1 -DHAVE_UNISTD_H=1 -DHAVE_STRING_H=1 -DHAVE_VARARGS_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FCNTL_H=1 -DHAVE_SETVBUF=1 -DHAVE_GETCWD=1 -DHAVE_BZERO=1 -DHAVE_RINDEX=1 -DHAVE_VFPRINTF=1 -DHAVE_VSPRINTF=1
LIBS = 
LOADLIBES = $(LIBS)

ALLOCA = 

SHELL = /bin/sh

CFLAGS = -g
LDFLAGS = -g

prefix = /usr/gnu
exec_prefix = ${prefix}
bindir = $(exec_prefix)/bin
# Prefix for each installed program, normally empty or `g'.
binprefix = 
libdir = $(prefix)/lib
# Prefix for each installed man page, normally empty or `g'.
manprefix = 
mandir = $(prefix)/man/man1
manext = 1
infodir = $(prefix)/info

# For info program. 
DEFAULT_INFOPATH = $(infodir):.

#### End of system configuration section. ####

VERSION = 3.1
DISTNAME = texinfo-$(VERSION)

# Subdirectories that have makefiles
SUBDIRS = libtxi makeinfo info util

# All subdirectories that go into a distribution
ALL_SUBDIRS = $(SUBDIRS) emacs

MDEFINES = bindir='$(bindir)' mandir='$(mandir)' manext='$(manext)' \
	prefix='$(prefix)' binprefix='$(binprefix)' \
	manprefix='$(manprefix)' infodir='$(infodir)' CFLAGS='$(CFLAGS)' \
	CC='$(CC)' ALLOCA='$(ALLOCA)' LDFLAGS='$(LDFLAGS)' \
	DEFAULT_INFOPATH='$(DEFAULT_INFOPATH)'

all: sub-all texi.info

install: all installdirs
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
	d=$(srcdir); test -f ./texi.info && d=.; \
	cd $$d; for f in texi.info* ; do $(INSTALL_DATA) $$f $(infodir)/$$f; done

installdirs:
	-sh $(srcdir)/util/mkinstalldirs $(bindir) $(datadir) $(libdir) $(infodir) $(mandir)

uninstall:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
	rm -f $(infodir)/texi.info*

Makefile: Makefile.in config.status
	sh ./config.status

config.status: configure
	$(srcdir)/configure --srcdir=$(srcdir) --no-create

configure: configure.in
	cd $(srcdir); autoconf

sub-all TAGS:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done

clean mostlyclean:
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done

distclean: clean
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) $@; cd ..; done
	rm -f Makefile config.status

realclean: 
	for dir in $(SUBDIRS); do echo making $@ in $$dir; cd $$dir; $(MAKE) $(MDEFINES) realclean; cd ..; done
	rm -f TAGS texi.info* Makefile config.status configure

texi.info:
	./makeinfo/makeinfo -I$(srcdir) texi.texi

texinfo.dvi:
	PATH="$(srcdir)/util:$${PATH}" TEXINPUTS="$(srcdir):$(common):$${TEXINPUTS}" texi2dvi $(srcdir)/texi.texi

dist: DISTFILES
	rm -rf $(DISTNAME)
	mkdir $(DISTNAME)
	for d in $(ALL_SUBDIRS); do mkdir $(DISTNAME)/$$d; done
	for f in `cat DISTFILES`; do \
	   ln $(srcdir)/$$f $(DISTNAME)/$$f || { echo copying $$f; cp -p $(srcdir)/$$f $(DISTNAME)/$$f ; } \
	done
	tar --gzip -chvf $(DISTNAME).tar.z $(DISTNAME)
	rm -rf $(DISTNAME)

# Gets rid of most of the unwanted files.  Verify manually (if necessary)
# that this produces a list of all the files desired in the distribution. 
DISTFILES: force
	(cd $(srcdir); find . $(ALL_SUBDIRS) ! -type d -print) \
	| sed  '/\/RCS\//d; \
	       /\.tar.*/d; \
	       /~$$/d; /\.o$$/d; \
	       /\/config\.status$$/d; \
	       /\/Makefile$$/d;	\
	       /\/.*\.BAK$$/d; \
	       /\/core$$/d; \
	       /\/a.out$$/d; \
	       /\/=/d; \
	       /\/conftest\.c$$/d; \
	       /\/DISTFILES$$/d; \
	       /\.toc$$/d; \
	       /\.aux$$/d; /\.log$$/d; \
	       /\.cps$$/d; /\.cp$$/d; \
	       /\.fns$$/d; /\.fn$$/d; \
	       /\.tps$$/d; /\.tp$$/d; \
	       /\.vrs$$/d; /\.vr$$/d; \
	       /\.pgs$$/d; /\.pg$$/d; \
	       /\.kys$$/d; /\.ky$$/d; \
	       s/^.\///; /^\.$$/d;' \
	| sort | uniq > DISTFILES

force:

pkg:    pkg_bin

pkg_bin: install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST texinfo_bin

# Prevent GNU make v3 from overflowing arg limit on SysV.
.NOEXPORT:
