# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's src directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ..

srcdir = .
VPATH = .

include $(TOPDIR)/Makeconf

srcdir = .
VPATH = .:./../bsd-math

# Can this be fixed?

INCFLAGS = -I. -I$(srcdir)/../liboctave -I$(srcdir)/../dld -I$(srcdir)/.. -I$(srcdir) -I$(TOPDIR)/liboctave -I$(TOPDIR)/dld -I$(TOPDIR)

LIBOBJS = 

# Cancel default rule for .c from .y and make .cc from .y instead.

%.c : %.y

%.cc : %.y
	$(YACC) $(YFLAGS) $<
	mv y.tab.c $(@F)

# Likewise for .c from .l.

%.c : %.l

%.cc : %.l
	$(LEX) $(LFLAGS) $< > $(@F)

# Here is a rule for generating dependencies for .cc files:

%.d: %.cc
	rm -f $@
	if test "$(srcdir)" = "." ; then \
	  $(CXX) -MM $(CPPFLAGS) $(ALL_CXXFLAGS) $< | \
	    sed -e 's/$*\.o/& $@/g' > $@.tmp && \
	    mv $@.tmp $@ ; \
	else \
	  $(CXX) -MM $(CPPFLAGS) $(ALL_CXXFLAGS) $< | \
	    sed -e 's/$*\.o/& $@/g' -e 's,$(srcdir)/,,g' > $@.tmp && \
	    mv $@.tmp $@ ; \
	fi

INCLUDES = arith-ops.h builtins.h dynamic-ld.h error.h file-io.h \
	g-builtins.h gripes.h help.h idx-vector.h input.h lex.h \
	mappers.h missing-math.h octave.h octave-hist.h pager.h parse.h \
	pr-output.h procstream.h sighandlers.h statdefs.h symtab.h \
	sysdep.h t-builtins.h tree-base.h tree-const.h tree.h \
	unwind-prot.h user-prefs.h utils.h variables.h version.h \
	xdiv.h xpow.h SLStack.h Stack.h

SOURCES = arith-ops.cc builtins.cc dynamic-ld.cc error.cc file-io.cc \
	g-builtins.cc gripes.cc help.cc idx-vector.cc input.cc lex.cc \
	mappers.cc octave.cc octave-hist.cc pager.cc parse.cc \
	pr-output.cc procstream.cc sighandlers.cc symtab.cc \
	sysdep.cc t-builtins.cc tc-assign.cc tc-colloc.cc \
	tc-dassl.cc tc-det.cc tc-eig.cc tc-extras.cc tc-fft.cc \
	tc-fsolve.cc tc-fsqp.cc tc-hess.cc tc-ifft.cc tc-index.cc \
	tc-inlines.cc tc-inv.cc tc-lpsolve.cc tc-lsode.cc tc-lu.cc \
	tc-npsol.cc tc-qpsol.cc tc-qr.cc tc-quad.cc tc-rand.cc \
	tc-schur.cc tc-svd.cc tree.cc tree-const.cc tree-plot.cc \
	unwind-prot.cc user-prefs.cc utils.cc variables.cc xdiv.cc \
	xpow.cc

DEP_SOURCES_1 = $(patsubst %.l, %.cc, $(SOURCES))
DEP_SOURCES = $(patsubst %.y, %.cc, $(DEP_SOURCES_1))
MAKEDEPS = $(patsubst %.cc, %.d, $(DEP_SOURCES))

DLD_OBJECTS = tc-colloc.o tc-dassl.o tc-det.o tc-eig.o \
	tc-fft.o tc-fsolve.o tc-fsqp.o tc-hess.o tc-ifft.o tc-inv.o \
	tc-lpsolve.o tc-lsode.o tc-lu.o tc-npsol.o tc-qpsol.o tc-qr.o \
	tc-quad.o tc-rand.o tc-schur.o tc-svd.o

OBJECTS = arith-ops.o builtins.o error.o file-io.o g-builtins.o \
	gripes.o help.o idx-vector.o input.o lex.o mappers.o \
	octave.o octave-hist.o pager.o parse.o pr-output.o \
	procstream.o sighandlers.o symtab.o sysdep.o t-builtins.o \
	tc-assign.o tc-extras.o tc-index.o tree.o tree-const.o \
	tree-plot.o unwind-prot.o user-prefs.o utils.o variables.o \
	xdiv.o xpow.o \
	

OCTAVE_LIBS = ../liboctave.a ../libcruft.a ../libreadline.a 

DISTFILES = Makefile.in parse.cc lex.cc y.tab.h \
	$(INCLUDES) $(SOURCES)

all: octave
.PHONY: all

octave: $(DLD_OBJECTS) $(OBJECTS) $(OCTAVE_LIBS) $(LIBOBJS)
	$(CXX) $(CPPFLAGS) $(ALL_CXXFLAGS) $(ALL_LDFLAGS) -o octave \
	$(OBJECTS) $(DLD_OBJECTS) \
	$(OCTAVE_LIBS) \
	$(LIBOBJS) \
	$(FLIBS) $(LEXLIB) -ltermcap -lm -lg++

check: all
.PHONY: check

install: all
	if test -d $(bindir) ; then true ; else $(TOPDIR)/mkpath $(bindir) ; fi
	rm -f $(bindir)/octave
	$(INSTALL) octave $(bindir)/octave-$(version)
	ln $(bindir)/octave-$(version) $(bindir)/octave
.PHONY: install

uninstall:
	rm -f $(bindir)/octave
.PHONY: uninstall

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

clean:
	rm -f *.a *.o
.PHONY: clean

mostlyclean:
	rm -f *.o
.PHONY: mostlyclean

distclean: clean
	rm -f Makefile octave .fname *.d
.PHONY: distclean

realclean: distclean
	rm -f tags TAGS y.tab.c y.tab.h yy.lex.c lex.cc parse.cc *.d
.PHONY: realclean

local-dist: parse.cc lex.cc
	ln $(DISTFILES) ../`cat ../.fname`/src
	rm -f parse.cc lex.cc y.tab.h
.PHONY: local-dist

dist: parse.cc lex.cc
	ln $(DISTFILES) ../`cat ../.fname`/src
	rm -f parse.cc lex.cc y.tab.h
.PHONY: dist

# Special rules -- these files need special things to be defined.

utils.o:
	$(CXX) -c $(CPPFLAGS) $(ALL_CXXFLAGS) \
	-DOCTAVE_HOME=\"$(prefix)\"  \
	-DDEFAULT_PAGER=\"$(DEFAULT_PAGER)\"  \
	$(srcdir)/utils.cc

y.tab.h: parse.cc

include $(MAKEDEPS)