# Generated automatically from Makefile.in by configure.
# ---------------------------------------------------------------------------
#    Makefile for KOSHER DILL - Disk space management utility
# ---------------------------------------------------------------------------

BINDIR 		= /usr/local/blt/bin
LIBDIR 		= /usr/local/blt
WISH 		= /usr/local/blt/bin/blt_wish

template 	= $(srcdir)/kosherdill.tmpl
script 		= kosherdill
INSTALL 	= /usr/bin/install -c
INSTALL_DATA 	= $(INSTALL) -m 644
INSTALL_PROGRAM = $(INSTALL)
SHELL 		= /bin/sh
RM 		= rm -f
srcdir		= .

all: kosherdill-build

install: kosherdill-install install-dirs install-library install-help
	$(INSTALL_PROGRAM) $(script) $(BINDIR)	
	@echo '== install complete =='

kosherdill-build: $(template)
	$(RM) $(script)
	sed -e "s#%install-dir%#`pwd`#" -e "s#%blt-program%#$(WISH)#" \
		$(template) > $(script)
	chmod 755 $(script)

kosherdill-install: $(template)
	$(RM) $(script)
	sed -e "s#%install-dir%#$(LIBDIR)#" \
		-e "s#%blt-program%#$(WISH)#" $(template) > $(script)
	chmod 755 $(script)

install-help:
	@set -x; for i in $(srcdir)/Help/[A-Z][a-z]* ; do \
	    $(INSTALL_DATA) $$i $(LIBDIR)/Help ; \
	done

install-library:
	@set -x; for i in $(srcdir)/scripts/*tcl* ; do \
	    $(INSTALL_DATA) $$i $(LIBDIR)/scripts ; \
	done

install-dirs:
	@for i in $(LIBDIR) $(LIBDIR)/scripts $(LIBDIR)/Help $(BINDIR) ; do \
	    if test ! -d $$i ; then \
		mkdir $$i ; \
	    fi ; \
	done

clean:
	$(RM) $(script) *\~ "#"*	

realclean: clean
	$(RM) config.status Makefile
