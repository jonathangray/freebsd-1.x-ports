#
# Makefile for NNTP intstallation
# $Header: /a/cvs/386BSD/ports/news/nntp/Makefile,v 1.3 1994/04/25 23:57:05 adam Exp $
# The program that knows how to deal with makefiles. [Uncomment if needed.]
# MAKE=make
# set CFLAGS to be -Ipath/to/special/includes if needed
CFLAGS= -O -I../dbz
# link with DBZ
LIBS=../libcnews.a -lcrypt

DISTFILES = Makefile README CHANGES

DISTDIRS = common doc dbz inews server support xfer xmit

DIST = $(DISTFILES) $(DISTDIRS)

SHELL=/bin/sh

# Make this either "server" or "client" depending on whether
# you want this package compiled to be a server or a client
# (You can always say "make server" or "make client" if you want.

TYPE = 

all: make_$(TYPE)

make_:
	@ echo "If you want to make a server version or a client version,"
	@ echo "type \"make server\" or \"make client\" respectively."
	@ echo "See README for details."

make_server: /nowhere
	cd dbz; $(MAKE) u
	cd server; $(MAKE) $(MFLAGS) "LIBS=${LIBS}" "CFLAGS=${CFLAGS}"
	cd support; $(MAKE) $(MFLAGS) "LIBS=${LIBS}" "CFLAGS=${CFLAGS}"
	cd doc; $(MAKE) $(MFLAGS) "LIBS=${LIBS}" "CFLAGS=${CFLAGS}"
	cd xmit; $(MAKE) $(MFLAGS) "LIBS=${LIBS}" "CFLAGS=${CFLAGS}"
	cd xfer; $(MAKE) $(MFLAGS) "LIBS=${LIBS}" "CFLAGS=${CFLAGS}"

server: make_server

make_client: /nowhere
	cd inews; $(MAKE) $(MFLAGS) "LIBS=${LIBS}" "CFLAGS=${CFLAGS}"
	cd doc; $(MAKE) $(MFLAGS) "LIBS=${LIBS}" "CFLAGS=${CFLAGS}"

client: make_client

install: install_$(TYPE)

install_:
	@ echo "If you want to install a server version or"
	@ echo "a client version, type \"make install_server\""
	@ echo "or \"make install_client\" respectively."
	@ echo "See README for more details."

install_server:
	cd server; make install
	cd support; make install
	cd doc; make install

install_client:
	cd inews; make install
	cd doc; make install

clean:
	rm  -f .[a-z]* ; for i in $(DISTDIRS); do cd $$i; $(MAKE) $(MFLAGS) clean; cd ..; done

check:
	for i in $(DISTDIRS); do cd $$i; $(MAKE) $(MFLAGS) check; cd ..; done

distrib: check
	mkdir ../nntp.dist
	cp -rp $(DIST) ../nntp.dist
	cd ../nntp.dist; for i in $(DISTDIRS); do cd $$i; $(MAKE) $(MFLAGS) distrib; cd ..; done

rpt: /nowhere
	tar cf rrnpatches.tar rrnpatches; compress rrnpatches.tar

server: /nowhere
client: /nowhere

/nowhere: ;
