# PD Bourne/Korn Shell
# $Id: Makefile,v 1.2 1994/04/17 00:57:54 sean Exp $

SHELL = /bin/sh
MAKE  = make
CC=gcc -O
LN=ln -s
#LN=ln
#CONFIG= -D_SYSV
#CONFIG= -D_BSD 
#CONFIG= -D_BSD -DHAVE_SYS_STDTYPES
#CONFIG= -D_V7
#CONFIG= -D_ST				# Atari ST
#CONFIG= -D_BSDI -D_POSIX_TERM		# BSD/386
CONFIG= -D_BSD -D_POSIX_TERM		# 386bsd
MANPAGES = ksh.1
#MANDIR=/usr/catman/u_man/man1
#MANDIR=/usr/man/man1
DESTDIR=/usr/local
MANDESTDIR=$(DESTDIR)/man

#INSTALL=bsdinstall
INSTALL=install

all:	ksh

ksh:	libs
	( cd sh ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )

libs:
	( cd std ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' libs )

install: sh/ksh inst-man inst-etc
	( cd sh ; $(INSTALL) -s ksh $(DESTDIR)/bin )

sh/ksh:	ksh

inst-man: $(MANPAGES)
	$(INSTALL) -c -m 444 $(MANPAGES) $(MANDESTDIR)/man1

inst-etc:
	$(INSTALL) -c -m 444 etc/ksh.kshrc /etc

clean clobber:
	( cd std ; $(MAKE) $@ )
	( cd sh ; $(MAKE) $@ )
	-rm -f *.out



package:	pkg_bin

pkg_bin: install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST ksh_bin
