# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's src directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ..

include $(TOPDIR)/Makeconf

srcdir = .
VPATH = .:./../bsd-math

# Can this be fixed?

INCFLAGS = -I. -I$(srcdir)/../liboctave -I$(srcdir)/.. -I$(srcdir) -I$(TOPDIR)/liboctave -I$(TOPDIR)

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
	-if test "$(srcdir)" = "." ; then \
	  $(CXX) -MM $(CPPFLAGS) $(ALL_CXXFLAGS) $< | \
	    sed -e 's/$*\.o/& $@/g' > $@.tmp && \
	    mv $@.tmp $@ ; \
	else \
	  $(CXX) -MM $(CPPFLAGS) $(ALL_CXXFLAGS) $< | \
	    sed -e 's/$*\.o/& $@/g' -e 's,$(srcdir)/,,g' > $@.tmp && \
	    mv $@.tmp $@ ; \
	fi

# And one for .c files.too:

%.d: %.c
	rm -f $@
	-if test "$(srcdir)" = "." ; then \
	  $(CC) -MM $(CPPFLAGS) $(ALL_CFLAGS) $< | \
	    sed -e 's/$*\.o/& $@/g' > $@.tmp && \
	    mv $@.tmp $@ ; \
	else \
	  $(CC) -MM $(CPPFLAGS) $(ALL_CFLAGS) $< | \
	    sed -e 's/$*\.o/& $@/g' -e 's,$(srcdir)/,,g' > $@.tmp && \
	    mv $@.tmp $@ ; \
	fi

INCLUDES = arith-ops.h builtins.h dynamic-ld.h defaults.h.in \
	defaults.h error.h f-balance.h f-chol.h f-colloc.h f-dassl.h \
	f-det.h f-eig.h f-expm.h f-fft.h f-fsolve.h f-fsqp.h f-givens.h \
	f-hess.h f-ifft.h f-inv.h f-lpsolve.h f-lsode.h f-lu.h \
	f-npsol.h f-qpsol.h f-qr.h f-quad.h f-qzval.h f-rand.h \
	f-schur.h f-svd.h f-syl.h file-io.h g-builtins.h getopt.h \
	gripes.h help.h idx-vector.h input.h lex.h mappers.h \
	missing-math.h octave.h octave-hist.h pager.h parse.h \
	pr-output.h procstream.h sighandlers.h statdefs.h symtab.h \
	sysdep.h t-builtins.h token.h tree-base.h tree-const.h tree.h \
	unwind-prot.h user-prefs.h utils.h variables.h version.h \
	xdiv.h xpow.h SLStack.h Stack.h

SOURCES = arith-ops.cc builtins.cc dynamic-ld.cc error.cc f-chol.cc \
	f-colloc.cc f-balance.cc f-dassl.cc f-det.cc f-eig.cc \
	f-expm.cc f-fft.cc f-fsolve.cc f-fsqp.cc f-givens.cc \
	f-hess.cc f-ifft.cc f-inv.cc f-lpsolve.cc f-lsode.cc f-lu.cc \
	f-npsol.cc f-qpsol.cc f-qr.cc f-quad.cc f-qzval.cc f-rand.cc \
	f-schur.cc f-svd.cc f-syl.cc file-io.cc	g-builtins.cc \
	getopt.c getopt1.c gripes.cc help.cc idx-vector.cc input.cc \
	lex.l mappers.cc octave.cc octave-hist.cc pager.cc parse.y \
	pr-output.cc procstream.cc sighandlers.cc symtab.cc \
	sysdep.cc t-builtins.cc tc-assign.cc tc-extras.cc \
	tc-index.cc tc-inlines.cc token.cc tree.cc tree-const.cc \
	tree-plot.cc unwind-prot.cc user-prefs.cc utils.cc \
	variables.cc xdiv.cc xpow.cc

DEP_SOURCES_2 = $(patsubst %.l, %.cc, $(SOURCES))
DEP_SOURCES_1 = $(patsubst %.y, %.cc, $(DEP_SOURCES_2))
DEP_SOURCES = $(patsubst %.c, %.d, $(DEP_SOURCES_1))
MAKEDEPS = $(patsubst %.cc, %.d, $(DEP_SOURCES))

DLD_OBJECTS = f-balance.o f-chol.o f-colloc.o f-dassl.o f-det.o \
	f-eig.o f-expm.o f-fft.o f-fsolve.o f-fsqp.o f-givens.o \
	f-hess.o f-ifft.o f-inv.o f-lpsolve.o f-lsode.o f-lu.o \
	f-npsol.o f-qpsol.o f-qr.o f-quad.o f-qzval.o f-rand.o \
	f-schur.o f-svd.o f-syl.o 

OBJECTS = arith-ops.o builtins.o error.o file-io.o g-builtins.o \
	getopt.o getopt1.o gripes.o help.o idx-vector.o input.o \
	lex.o mappers.o octave.o octave-hist.o pager.o parse.o \
	pr-output.o procstream.o sighandlers.o symtab.o sysdep.o \
	t-builtins.o tc-assign.o tc-extras.o tc-index.o token.o \
	tree.o tree-const.o tree-plot.o unwind-prot.o user-prefs.o \
	utils.o variables.o xdiv.o xpow.o \
	

OCTAVE_LIBS = ../liboctave.a ../libcruft.a ../libinfo.a \
	-lreadline

DISTFILES = Makefile.in move-if-change parse.cc lex.cc y.tab.h \
	$(INCLUDES) $(SOURCES)

all: defaults.h octave
.PHONY: all

octave:	$(DLD_OBJECTS) $(OBJECTS) $(OCTAVE_LIBS) $(LIBOBJS)
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

defaults.h: ../Makeconf defaults.h.in
	@echo "Making defaults.h from defaults.h.in..."
	@(sed < $(srcdir)/defaults.h.in > defaults.h.tmp \
	-e 's;%DEFAULT_PAGER%;\"${DEFAULT_PAGER}\";' \
	-e 's;%OCTAVE_HOME%;\"${prefix}\";')
	@$(srcdir)/move-if-change defaults.h.tmp defaults.h

include $(MAKEDEPS)
