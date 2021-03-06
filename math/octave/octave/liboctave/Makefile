# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's liboctave directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ..

srcdir = .
VPATH = .

include $(TOPDIR)/Makeconf

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

INCLUDES = Bounds.h CollocWt.h DAE.h DAEFunc.h FEGrid.h FSQP.h \
	LinConst.h LP.h LPsolve.h Matrix.h NLConst.h NLEqn.h \
	NLFunc.h NLP.h NPSOL.h ODE.h ODEFunc.h Objective.h QLD.h \
	QP.h QPSOL.h Quad.h Range.h f77-uscore.h sun-utils.h

SOURCES = Bounds.cc ColVector.cc CollocWt.cc DAE.cc DAEFunc.cc \
	DiagMatrix.cc FEGrid.cc FSQP.cc LinConst.cc LP.cc LPsolve.cc \
	Matrix-ext.cc Matrix.cc NLConst.cc NLEqn.cc NLFunc.cc NPSOL.cc \
	Objective.cc ODE.cc ODEFunc.cc QLD.cc QP.cc QPSOL.cc Quad.cc \
	Range.cc RowVector.cc sun-utils.cc

EXTRAS = mx-inlines.cc

DISTFILES = Makefile.in $(SOURCES) $(INCLUDES) $(EXTRAS)

MAKEDEPS = $(patsubst %.cc, %.d, $(SOURCES))

OBJECTS = $(patsubst %.cc, %.o, $(SOURCES))

LIBOCTAVE_DEPEND := $(patsubst %, ../liboctave.a(%), $(OBJECTS))

all: ../liboctave.a
.PHONY: all

../liboctave.a: $(OBJECTS)
	$(AR) $(ARFLAGS) ../liboctave.a $(OBJECTS)
	$(RANLIB) ../liboctave.a

check: all
.PHONY: check

install: all
	if test -d $(libdir) ; then true ; else $(TOPDIR)/mkpath $(libdir) ; fi
	rm -f $(libdir)/liboctave.a
	$(INSTALL_DATA) ../liboctave.a $(libdir)/liboctave.a
	$(RANLIB) $(libdir)/liboctave.a
	if test -d $(includedir) ; then true ; \
	else $(TOPDIR)/mkpath $(includedir) ; fi
	for f in $(INCLUDES) ; do \
	  rm -f $(includedir)/$$f ; \
	  $(INSTALL_DATA) $(srcdir)/$$f $(includedir)/$$f ; \
	done
.PHONY: install

uninstall:
	rm -f $(libdir)/liboctave.a
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
	rm -f Makefile  ../liboctave.a $(TMPSRC) $(TMPINC) *.d
.PHONY: distclean

realclean: distclean
	rm -f tags TAGS *.d
.PHONY: realclean

local-dist:
	ln $(DISTFILES) ../`cat ../.fname`/liboctave
.PHONY: local-dist

dist:
	ln $(DISTFILES) ../`cat ../.fname`/liboctave
.PHONY: dist

include $(MAKEDEPS)
