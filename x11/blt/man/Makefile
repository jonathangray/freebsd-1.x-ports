# Generated automatically from Makefile.in by configure.
# ------------------------------------------------------------------------
# 	Makefile for manual page files 
# ------------------------------------------------------------------------

MAN_SECT 	= n
TOPDIR  	= /opt/cellar/deli/blt
MANDIR  	= /opt/cellar/deli/blt/man
SECT_DIR 	= $(MANDIR)/man$(MAN_SECT)
SHELL    	= /bin/sh
RM		= rm -rf
srcdir		= .

all:

install: install-dirs
	@for i in $(srcdir)/*.man ; do \
	    file=$(SECT_DIR)/blt_`basename $$i .man`.$(MAN_SECT) ; \
	    echo "installing manual $$file" ; \
	    rm -f $$file ; \
	    sed -e "/man\.macros/r $(srcdir)/man.macros" -e '/man\.macros/d' \
		$$i > $$file ; \
	    chmod 444 $$file; \
	done 

install-dirs:
	@for i in $(TOPDIR) $(MANDIR) $(SECT_DIR) ; do \
	    if test ! -d $$i ; then \
		mkdir $$i ; \
	    fi ; \
        done

clean:
	$(RM) *\~ "#"*

