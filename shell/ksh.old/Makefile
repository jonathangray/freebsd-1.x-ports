# PD Bourne/Korn Shell
# Makefile,v 1.3 1993/07/13 07:49:30 cgd Exp

SHELL = /bin/sh
MAKE  = make
CC=gcc -pipe -g -O
LN=ln -s
#LN=ln
#CONFIG= -D_SYSV
CONFIG= -D_BSD 
#CONFIG= -D_BSD -DHAVE_SYS_STDTYPES
#CONFIG= -D_V7
#CONFIG= -D_ST		/* Atari ST */

MANPAGES = ksh.1
#MANDIR=/usr/catman/u_man/man1
#MANDIR=/usr/man/man1

ETCFILES= etc/ksh.kshrc

#INSTALL=bsdinstall
INSTALL=install

DESTDIR=/usr/local
MANDESTDIR=/usr/local/man

all:	ksh

ksh:	libs
	( cd sh ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )

libs:
	( cd std ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' libs )

install: inst-ksh inst-man inst-etc

inst-ksh: sh/ksh
	( cd sh ; $(INSTALL) -c -s ksh $(DESTDIR)/bin )

sh/ksh:	ksh

inst-man: $(MANPAGES)
	$(INSTALL) -c -m 444 $(MANPAGES) $(MANDESTDIR)/man1

inst-etc:
	$(INSTALL) -c -m 444 $(ETCFILES) /etc
	@echo "updating /etc/shells"
	@cp /etc/shells /etc/shells.bak
	@(grep -v $(DESTDIR)/bin/ksh /etc/shells.bak; echo $(DESTDIR)/bin/ksh) \
		> /etc/shells

clean:
	( cd std ; $(MAKE) $@ )
	( cd sh ; $(MAKE) $@ )
	-rm -f *.out

cleandir:	clean


pkg:	pkg_src pkg_bin

pkg_src: pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S ksh_src

pkg_bin: pkg/+DESC_B pkg/+ONELINE_B pkg/+PLIST_B pkg/+INST_B pkg/+DINST_B
	pkg_create -c pkg/+ONELINE_B -d pkg/+DESC_B -f pkg/+PLIST_B -i pkg/+INST_B -k pkg/+DINST_B ksh_bin
