# Generated automatically from Makefile.in by configure.
### Start of system configuration section ###

srcdir = .
VPATH = .

CC = gcc
AR = ar
# set RANLIB = @: if your system doesn't have or need ranlib
RANLIB = ranlib
# use cp if you don't have install
INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

DEFS = -DHAVE_CONFIG_H

CFLAGS = -O2
LDFLAGS = -s
LIBS = 

prefix = /usr/local
exec_prefix = $(prefix)

bindir = $(exec_prefix)/bin

mandir = $(prefix)/man
man1dir = $(mandir)/man1
man1ext = .1

#############################################################################

SHELL	= /bin/sh

commondir = common
bsddir = bsd
clientdir = client

bsdlib = libbsdbits.a
commonlib = libcommon.a

MDEFINES = bindir='$(bindir)' configdir='..' \
	   commondir='../$(commondir)' bsddir='../$(bsddir)' clientdir='../$(clientdir)' \
	   bsdlib='$(bsdlib)' commonlib='$(commonlib)' \
	   mandir='$(mandir)' man1dir='$(man1dir)' man1ext='$(man1ext)' \
	   LIBS='$(LIBS)' AR='$(AR)' RANLIB='$(RANLIB)' \
	   INSTALL_PROGRAM='$(INSTALL_PROGRAM)' INSTALL_DATA='$(INSTALL_DATA)' \
	   DEFS='$(DEFS)' CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' \
	   LIBS='$(LIBS)' CC='$(CC)'

SUBDIRS = $(commondir) $(bsddir) $(clientdir)

all install uninstall clean mostlyclean distclean realclean depend::
	@for dir in $(SUBDIRS); do \
	  echo "making $@ in $$dir"; \
	  cd $$dir; \
	  if [ -f Makefile ]; then \
	    $(MAKE) $(MDEFINES) $@ || exit 1; \
	  fi; \
	  cd ..; \
	done

distclean::
	-rm -f Makefile config.h config.status

ClientTar:	distclean PackageClient

PackageClient:
	@cd ..;\
	 echo "running \`fspclient/package.client fsp' in `pwd`";\
	 fspclient/package.client fspclient

package: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST fspclient_bin

#############################################################################
