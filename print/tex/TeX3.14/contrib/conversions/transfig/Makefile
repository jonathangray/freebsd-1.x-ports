#
# Top level Makefile for TransFig
#
# to make fig2dev:         type "make"
#
# to install:              set  BINDIR in Makefile
#			   type "make install"
#
# to make manual:          type "make manual"

# directory in which to install executables
BINDIR = /usr/local/bin
MANDIR = /usr/man/manl

# for gcc
GFLAGS = -O -traditional -fstrength-reduce -fpcc-struct-return

all:
	cd transfig; make CC=$(CC) CFLAGS="$(CFLAGS)"
	cd fig2dev; make CC=$(CC) CFLAGS="$(CFLAGS)"

manual: 
	cd doc/manual; make; latex manual; latex manual

install:	all
	install -m 555 -c -s transfig/transfig$(TARGET_ARCH) $(BINDIR)/transfig
	install -m 555 -c -s fig2dev/fig2dev$(TARGET_ARCH) $(BINDIR)/fig2dev
	install -m 555 -c fig2dev/fig2ps2tex $(BINDIR)/fig2ps2tex
	install -m 555 -c fig2dev/pic2tpic $(BINDIR)/pic2tpic

install.man:	
	install -m 444 -c doc/transfig.1 $(MANDIR)/transfig.l
	install -m 444 -c doc/fig2dev.1 $(MANDIR)/fig2dev.l

clean:
	cd transfig; make clean
	cd fig2dev; make clean
	cd doc/manual; make clean
