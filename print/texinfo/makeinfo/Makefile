# Generated automatically from Makefile.in by configure.
# Makefile for GNU makeinfo.  -*- Indented-Text -*-
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
VPATH = $(srcdir):$(common)

common = $(srcdir)/../libtxi

CC = gcc
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

LN	= ln
RM	= rm -f
TAR	= tar
MKDIR	= mkdir
COMPRESS= compress

DEFS =  -DSTDC_HEADERS=1 -DHAVE_UNISTD_H=1 -DHAVE_STRING_H=1 -DHAVE_VARARGS_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FCNTL_H=1 -DHAVE_SETVBUF=1 -DHAVE_GETCWD=1 -DHAVE_BZERO=1 -DHAVE_RINDEX=1 -DHAVE_VFPRINTF=1 -DHAVE_VSPRINTF=1
LIBS = -L../libtxi -ltxi 
LOADLIBES = $(LIBS)

SHELL = /bin/sh

CFLAGS = -g
LDFLAGS = -g

prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
# Prefix for each installed program, normally empty or `g'.
binprefix = 
libdir = $(prefix)/lib
# Prefix for each installed man page, normally empty or `g'.
manprefix = 
mandir = $(prefix)/man/man1
manext = 1
infodir = $(prefix)/info

#### End of system configuration section. ####

SRCS =  makeinfo.c
OBJS =  makeinfo.o

PROGS = makeinfo

all: $(PROGS) makeinfo.info
sub-all: all

.c.o:
	$(CC) -c $(CPPFLAGS) -I. -I$(srcdir) -I$(common) $(DEFS) $(CFLAGS) $<

makeinfo: $(OBJS) ../libtxi/libtxi.a
	$(CC) $(LDFLAGS) -o makeinfo $(OBJS) $(LOADLIBES)

makeinfo.o: makeinfo.c $(common)/getopt.h

makeinfo.info: makeinfo makeinfo.texi
	./makeinfo --no-split -I$(srcdir) makeinfo.texi

install: all
	for f in $(PROGS); do $(INSTALL_PROGRAM) $$f $(bindir)/$(binprefix)$$f; done
	-d=$(srcdir); test -f ./makeinfo.info && d=.; $(INSTALL_DATA) $$d/makeinfo.info $(infodir)/makeinfo.info

uninstall:
	for f in $(PROGS); do rm -f $(bindir)/$(binprefix)$$f; done
	rm -f $(infodir)/makeinfo.info

TAGS: $(SRCS)
	etags $(SRCS)

clean:
	rm -f *.o a.out core core.* $(PROGS)

mostlyclean: clean

distclean: clean
	rm -f Makefile config.status

realclean: distclean
	rm -f TAGS
	rm -f makeinfo.info

Makefile: Makefile.in ../config.status
	cd ..; sh config.status

# Prevent GNU make v3 from overflowing arg limit on SysV.
.NOEXPORT:

# eof
