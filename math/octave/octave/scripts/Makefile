# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's scripts directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ..

srcdir = .
VPATH = .

include $(TOPDIR)/Makeconf

SOURCES = computer.in *.m

DISTFILES = Makefile.in $(SOURCES)

MFILES = $(wildcard $(srcdir)/*.m)
MFILES_NO_DIR = $(notdir $(MFILES))

all: computer.m
.PHONY: all

computer.m: computer.in
	if test "$(target_host_type)" = unknown ; then \
	  sed -e "s/%target_host_type%/Hi Dave, I'm a HAL-9000/" \
	    $(srcdir)/computer.in > computer.m ; \
	else \
	  sed -e "s/%target_host_type%/$(target_host_type)/" \
	    $(srcdir)/computer.in > computer.m ; \
	fi

check: all
.PHONY: check

install: all
	if test -d $(libsubdir) ; then true ; \
	else $(TOPDIR)/mkpath $(libsubdir) ; fi
	for f in $(MFILES_NO_DIR) ; do \
	  rm -f $(libdir)/$$f ; \
	  $(INSTALL_DATA) $(srcdir)/$$f $(libsubdir)/$$f ; \
	done
	rm -f $(libsubdir)/computer.m
	$(INSTALL_DATA) computer.m $(libsubdir)/computer.m
.PHONY: install

uninstall:
	for f in $(MFILES_NO_DIR) ; do rm -f $(libsubdir)/$$f ; done
.PHONY: uninstall

clean:
	rm -f computer.m
.PHONY: clean

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

mostlyclean: clean
.PHONY: mostlyclean

distclean: clean
	rm -f Makefile
.PHONY: distclean

realclean: distclean
	rm -f tags TAGS
.PHONY: realclean

local-dist:
	ln $(DISTFILES) ../`cat ../.fname`/scripts
.PHONY: local-dist

dist:
	ln $(DISTFILES) ../`cat ../.fname`/scripts
.PHONY: dist
