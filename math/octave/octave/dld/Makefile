# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's dld directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ..

srcdir = .
VPATH = .

include $(TOPDIR)/Makeconf

OBJECTS = dld.o find_exec.o define.o get_func.o get_symbol.o \
	list_undef.o mk_dummy.o ref.o ul_file.o ul_symbol.o \
	remove.o error.o

INCLUDES = dld.h defs.h

DISTFILES = Makefile.in $(SOURCES) $(INCLUDES)

all: ../libdld.a
.PHONY: all

../libdld.a: $(OBJECTS)
	$(AR) $(ARFLAGS) ../libdld.a $(OBJECTS)
	$(RANLIB) ../libdld.a

all: ../libdld.a

check: all
.PHONY: check

install: all
	if test -d $(libdir) ; then true ; else mkdir $(libdir) ; fi
	rm -f $(libdir)/libdld.a
	$(INSTALL_DATA) ../libdld.a $(libdir)/libdld.a
	if test -d $(includedir) ; then true ; else mkdir $(includedir) ; fi
	$(INSTALL_DATA) $(srcdir)/dld.h $(includedir)/dld.h
.PHONY: install

uninstall:
	rm -f $(libdir)/libdld.a
	for f in $(INCLUDES) ; do rm -f $(includedir)/$$f ; done
.PHONY: uninstall

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

clean:
	rm -f *.a *.o
.PHONY: clean

mostlyclean: clean
.PHONY: mostlyclean

distclean: clean
	rm -f Makefile  ../libdld.a
.PHONY: distclean

realclean: distclean
	rm -f tags TAGS
.PHONY: realclean

local-dist:
	ln $(DISTFILES) ../`cat ../.fname`/dld
.PHONY: local-dist

dist:
	ln $(DISTFILES) ../`cat ../.fname`/dld
.PHONY: dist

