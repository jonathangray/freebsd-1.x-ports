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

version = 1.14
srcs = zircon
objs = dccget.o dccsend.o
libs = lib
aux = zircon.ad rc
others = FAQ README CHANGES COPYRIGHT DOTzirconrc MANUAL Makefile \
	 dccget.c dccsend.c
bitmaps = bitmaps
distrib = $(srcs) $(libs) $(bitmaps) $(aux) $(others)

#LIBS =  -lsocket -linet -lnsl -lseq 
#
# Solaris....
#LIBS= -lsocket -lnsl -lelf -lucb


all: zircon dccget dccsend

zircon: Makefile tclIndex
	mv zircon zircon.src
	echo '#!$(WISH) -f' > zircon
	echo 'set zircon(lib) $(LIBDIR)' >> zircon
	tail +3 zircon.src >> zircon
	chmod a+rx zircon
	rm -f zircon.src

dccget:	dccget.o
	$(CC) -o dccget -s -O dccget.c $(LIBS)

dccsend:	dccsend.o
	$(CC) -o dccsend -s -O dccsend.c $(LIBS)

tclIndex:
	cd lib ;  echo 'auto_mkindex . *.tcl ; exit' | $(WISH)

install: zircon tclIndex dccget dccsend $(libs) $(bitmaps) $(aux) Makefile
	@if [ -d $(BINDIR) ]; then set +x; \
	else (set -x; mkdir $(BINDIR)); fi
	cp zircon $(BINDIR)
	@if [ -d $(LIBDIR) ]; then set +x; \
	else (set -x; mkdir $(LIBDIR)); fi
	cp -r dccget dccsend $(libs) $(bitmaps) $(LIBDIR)
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

pkg:	pkg_src pkg_bin

pkg_src: pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S pkg/+REQ_S clean
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S -r pkg/+REQ_S zircon_src

pkg_bin: pkg/+DESC_B pkg/+ONELINE_B pkg/+PLIST_B pkg/+REQ_B all
	pkg_create -c pkg/+ONELINE_B -d pkg/+DESC_B -f pkg/+PLIST_B -r pkg/+REQ_B zircon_bin
