#
# Makefile for building a standalone send-pr.
#
RELEASE=FreeBSD-1.1

prefix = /usr/gnu
bindir = $(prefix)/bin
datadir = $(prefix)/lib
infodir = $(prefix)/info
lispdir = $(prefix)/emacs/lisp
mandir = $(prefix)/man
man1dir = $(mandir)/man1

all: send-pr send-pr.el install-sid

send-pr: send-pr.sh
	sed -e 's,@DATADIR@,$(datadir),g' \
	    -e 's/@DEFAULT_RELEASE@/$(RELEASE)/g' send-pr.sh > send-pr

send-pr.el: send-pr-el.in
	sed -e 's,@DATADIR@,$(datadir),g' \
	    -e 's/@DEFAULT_RELEASE@/$(RELEASE)/g' send-pr-el.in > send-pr.el

install-sid: install-sid.sh
	sed -e 's,@BINDIR@,$(bindir),g' install-sid.sh > install-sid

install: all
	if [ -d $(prefix) ]; then true ; else mkdir $(prefix) ; fi
	if [ -d $(bindir) ]; then true ; else mkdir $(bindir) ; fi
	cp send-pr $(bindir)
	chmod 755 $(bindir)/send-pr
	cp install-sid $(bindir)
	chmod 755 $(bindir)/install-sid
	-parent=`echo $(lispdir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	if [ -d $(lispdir) ] ; then true ; else mkdir $(lispdir) ; fi
	cp send-pr.el $(lispdir)/send-pr.el
	chmod 644 $(lispdir)/send-pr.el
	cp send-pr.info $(infodir)/send-pr.info
	chmod 644 $(infodir)/send-pr.info
	if [ -d $(datadir) ] ; then true ; else mkdir $(datadir) ; fi
	if [ -d $(datadir)/gnats ] ; then true ; else mkdir $(datadir)/gnats ; fi
	cp categories $(datadir)/gnats/cdrom
	chmod 644 $(datadir)/gnats/cdrom
	-parent=`echo $(man1dir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	if [ -d $(man1dir) ] ; then true ; else mkdir $(man1dir) ; fi
	cp send-pr.1 $(man1dir)/send-pr.1
	chmod 644 $(man1dir)/send-pr.1

clean:
	rm -f send-pr send-pr.el install-sid

package pkg_bin:
	pkg_create -t ./inst.XXXXXX -c pkg/+ONELINE -d pkg/+DESC \
	-f pkg/+PLIST send-pr_bin
