# This file was generated automatically by configure.  Do not edit.
VPATH = .
links = 
host_alias = i386-unknown-bsd
host_cpu = i386
host_vendor = unknown
host_os = bsd
host_canonical = i386-unknown-bsd
target_alias = i386-unknown-bsd
target_cpu = i386
target_vendor = unknown
target_os = bsd
target_canonical = i386-unknown-bsd
srcdir = .
prefix = /usr/local

exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
tooldir = $(exec_prefix)/i386-unknown-bsd

datadir = $(exec_prefix)/lib/dejagnu
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info
includedir = $(prefix)/include
docdir = $(datadir)/doc
targetdir = $(datadir)/$(target_canonical)

SHELL = /bin/sh

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

CFLAGS = -g
SUBDIRS = calc
RUNTEST = runtest
RUNTESTFLAGS = 

FLAGS_TO_PASS = \
	"CC=$(CC)" \
	"CFLAGS=$(CFLAGS)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"LDFLAGS=$(LDFLAGS)" \
	"MAKEINFO=$(MAKEINFO)" \
	"RUNTEST=$(RUNTEST)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)"

#### host, target, and site specific Makefile frags come in here.

all:	 subdirs

.PHONY: check installcheck
check:
installcheck:

.NOEXPORT:
MAKEOVERRIDES=

check:	force	
	rootme=`pwd`; export rootme; $(MAKE) DO=check DODIRS=calc subdir_do
		
subdir_do: force
	@for i in $(DODIRS); do \
	    if [ -d ./$$i ] ; then \
		if (rootme=`pwd`/ ; export rootme ; \
		    rootsrc=`cd $(srcdir); pwd`/ ; export rootsrc ; \
		    cd ./$$i; \
		    $(MAKE) $(FLAGS_TO_PASS) $(DO)) ; then true ; \
		else exit 1 ; fi ; \
	    else true ; fi ; \
	done
force:


subdirs:
	@for dir in ${SUBDIRS}; \
	do \
		echo "$$dir:"; \
		if [ -d $$dir ]; then \
			(rootme=`pwd`/ ; export rootme ; \
			 rootsrc=`cd $(srcdir); pwd`/ ; export rootsrc ; \
			 cd $$dir; $(MAKE) $(FLAGS_TO_PASS)); \
		fi; \
	done

clean:
	-rm -f *~ core *.o a.out xgdb *.x
		for dir in ${SUBDIRS}; \
		do \
			echo "$$dir:"; \
			if [ -d $$dir ]; then \
				(cd $$dir; $(MAKE) clean); \
			fi; \
		done

distclean: clean
	-rm -f *~ core
	-rm -f Makefile config.status *-init.exp site.*
	-rm -fr *.log summary detail
		for dir in ${SUBDIRS}; \
		do \
			echo "$$dir:"; \
			(cd $$dir; $(MAKE) distclean); \
		done

Makefile : $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

