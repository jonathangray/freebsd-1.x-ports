# Makefile for dvi2tty and disdvi       23/01/89   M.J.E. Mol
#
# For BSD Unix use the following CFLAGS definition
# CFLAGS = -Dstrchr=index
#
# This Makefile does not work for MSDOS. Make your 
# own one, or compile by hand.
#
CFLAGS=-Dstrchr=index
CC=$(CC_386BSD)
BINDIR=$(BINDIR_386BSD)
MANDIR=$(MANDIR_386BSD)
MANEXT=$(MANEXT_386BSD)
INSTALL=$(INSTALL_386BSD)

all:	dvi2tty disdvi

.SUFFIXES: .c .o

.c.o:
	$(CC) $(CFLAGS) -c $<

dvi2tty:dvi2tty.o dvistuff.o
	$(CC) $(CFLAGS) -o dvi2tty dvi2tty.o dvistuff.o

disdvi:disdvi.o
	$(CC) $(CFLAGS) -o disdvi disdvi.o

install: disdvi dvi2tty
	$(INSTALL) -m 755 -s disdvi dvi2tty $(BINDIR)
	$(INSTALL) -m 644 dvi2tty.1 $(MANDIR)/man$(MANEXT)

clean:
	rm -f *.o core.* a.out disdvi dvi2tty

veryclean: clean
