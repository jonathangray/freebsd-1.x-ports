# Generated automatically from Makefile.in by configure.
#
# Makefile for octave's libcruft/misc directory
#
# John W. Eaton
# jwe@che.utexas.edu
# Department of Chemical Engineering
# The University of Texas at Austin

TOPDIR = ../..

srcdir = .
VPATH = .

SPECIAL = gen-d1mach.c d1mach-tst.for dostop.c

SPECIAL_DEPEND = d1mach.o dostop.o

DISTFILES = $(subst d1mach.f, , $(DISTFILES))

EXTERNAL_DISTFILES = $(DISTFILES)

include $(TOPDIR)/Makeconf

include ../Makerules

d1mach.f: gen-d1mach
	./gen-d1mach > d1mach.f

# Don't optimize, to avoid gcc-2.3.3 problem.

XCC = $(subst -O, , $(CC))

gen-d1mach: $(srcdir)/gen-d1mach.c
	$(XCC) -DDP -o gen-d1mach $(srcdir)/gen-d1mach.c -lm
