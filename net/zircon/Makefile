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

#--------------------------------------------------
#
# you shouldn't modify anything below here
#
#--------------------------------------------------

version = 1.09
srcs = zircon
objs = dccget.o dccsend.o
libs = lib
aux = zircon.ad rc
others = README CHANGES COPYRIGHT DOTzirconrc MANUAL Makefile \
	 dccget.c dccsend.c
bitmaps = 
distrib = $(srcs) $(libs) $(bitmaps) $(aux) $(others)

#LIBS =  -lsocket -linet -lnsl -lseq 

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

install: zircon tclIndex dccget dccsend $(libs) $(aux) Makefile
	@if [ -d $(BINDIR) ]; then set +x; \
	else (set -x; mkdir $(BINDIR)); fi
	cp zircon $(BINDIR)
	@if [ -d $(LIBDIR) ]; then set +x; \
	else (set -x; mkdir $(LIBDIR)); fi
	cp -r dccget dccsend $(libs) $(LIBDIR)
	@if [ ! -f $(LIBDIR)/rc ]; then cp rc $(LIBDIR); \
	echo Remember to configure $(LIBDIR)/rc; fi
	@if [ ! -f $(LIBDIR)/zircon.ad ]; then cp zircon.ad $(LIBDIR); fi
	chmod a+r $(LIBDIR) $(LIBDIR)/* $(LIBDIR)/*/*
	chmod a+x $(LIBDIR) $(LIBDIR)/dccget $(LIBDIR)/dccsend $(LIBDIR)/lib

clean:
	rm -fr $(objs) dccsend dccget core errs zircon-$(version)

tar:	tclIndex
	rm -f zircon*.tar.[zZ]*
	mkdir zircon-$(version)
	cp -R $(distrib) zircon-$(version)
	tar chvf zircon-$(version).tar zircon-$(version)
	@gzip -9vf zircon-$(version).tar
	rm -rf zircon-$(version)

