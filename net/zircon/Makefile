#
# Makefile for Zircon
#
# Lindsay F. Marshall (Lindsay.Marshall@newcastle.ac.uk)
# 		Hacked from Makefile for TkMail which was
# 		hacked from Makefile for TkMan from Tom Phelps
#

# This Makefile includes the most critical and site specific
# settings needed to run Zircon.
#

# # # SITE SETTINGS # # #

WISH = /usr/local/bin/dpwish

# directory to put *.tk support scripts
LIBDIR = /usr/local/lib/zircon

# directory to put zircon executable script
BINDIR = /usr/local/bin

CC=cc
SHELL=/bin/sh

#--------------------------------------------------
#
# you shouldn't modify anything below here
#
#--------------------------------------------------

version = 1.15
srcs = zircon dccget.c dccsend.c crypt.c
objs = dccget.o dccsend.o crypt.o zping.o
execs = dccget dccsend crypt zping
libs = lib
index = lib/tclIndex
aux = zircon.ad rc
others = FAQ README CHANGES COPYRIGHT DOTzirconrc MANUAL Makefile TODO
bitmaps = bitmaps
distrib = $(index) $(srcs) $(libs) $(bitmaps) $(aux) $(others)

#LIBS =  -lsocket -linet -lnsl -lseq 
#
# Solaris....
#LIBS= -lsocket -lnsl -lelf -lucb


all: zircon $(execs)

zircon: Makefile $(index)
	mv zircon zircon.src
	echo '#!$(WISH) -f' > zircon
	echo 'set zircon(lib) $(LIBDIR)' >> zircon
	tail +3 zircon.src >> zircon
	chmod a+rx zircon
	rm -f zircon.src

dccget:	dccget.o
	$(CC) -o dccget -s -O dccget.c $(LIBS)

zping: zping.c
	$(CC) -o zping -s -O zping.c

crypt: crypt.c
	$(CC) -o crypt -s -O crypt.c

dccsend:	dccsend.o
	$(CC) -o dccsend -s -O dccsend.c $(LIBS)

$(index):
	cd lib ;  echo 'auto_mkindex . *.tcl ; exit' | $(WISH) -notk

install: all zircon $(libs) $(bitmaps) $(aux)
	@if [ -d $(BINDIR) ]; then set +x; \
	else (set -x; mkdir $(BINDIR)); fi
	cp zircon $(BINDIR)
	@if [ -d $(LIBDIR) ]; then set +x; \
	else (set -x; mkdir $(LIBDIR)); fi
	cp -r $(execs) $(libs) $(bitmaps) $(LIBDIR)
	@if [ ! -f $(LIBDIR)/rc ]; then cp rc $(LIBDIR); \
	echo Remember to configure $(LIBDIR)/rc; fi
	@if [ ! -f $(LIBDIR)/zircon.ad ]; then cp zircon.ad $(LIBDIR); fi
	chmod a+r $(LIBDIR) $(LIBDIR)/* $(LIBDIR)/*/*
	chmod a+x $(LIBDIR) $(LIBDIR)/dccget $(LIBDIR)/dccsend
	chmod a+x $(LIBDIR)/lib $(LIBDIR)/bitmaps

clean:
	rm -fr $(objs) core errs zircon-$(version) dccsend dccget

tar:	tclIndex
	rm -f zircon*.tar.gz
	mkdir zircon-$(version)
	cp -R $(distrib) zircon-$(version)
	tar chvf zircon-$(version).tar zircon-$(version)
	@gzip -9vf zircon-$(version).tar
	rm -rf zircon-$(version)

pkg:	pkg_bin

pkg_bin: pkg/+DESC pkg/+ONELINE pkg/+PLIST pkg/+REQ all install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST -r pkg/+REQ zircon_bin
