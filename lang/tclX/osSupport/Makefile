# Generated automatically from Makefile.in by configure.
#
# osSupport/Makefile.in --
#
# Makefile for Extended Tcl ossupport code.
#------------------------------------------------------------------------------
# Copyright 1992-1993 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# $Id: Makefile,v 1.2 1994/07/05 14:13:29 ache Exp $
#------------------------------------------------------------------------------
#
SHELL = /bin/sh

#------------------------------------------------------------------------------
# Autoconfig defines that can be overridden in Config.mk

CC          = cc
RANLIB      = ranlib
srcdir      = .
srcbasedir  = /usr/ports/lang/tclX
bldbasedir  = /usr/ports/lang/tclX
VPATH       = .
prefix      = /usr/local
exec_prefix = /usr/local
ARCH        = 
LIBOBJS     = 

#------------------------------------------------------------------------------
# Include user-editable defines.

.include "${bldbasedir}/Config.mk"

#------------------------------------------------------------------------------

LIBTCLX.A = ../tclmaster/lib${ARCH}/libtclx.a

CC_FLAGS = ${CPPFLAGS} ${XCFLAGS} ${CFLAGS} -I${srcbasedir}/src \
           -I${TCL_UCB_SRC}
 
.c.o:
	${CC} ${CC_FLAGS} -c $<

#------------------------------------------------------------------------------

all: made.tmp

made.tmp ${LIBTCLX.A}: ${LIBOBJS}
	if [ "${LIBOBJS}" != "" ] ; then \
	    ${AR} cvr ${LIBTCLX.A} ${LIBOBJS} ;\
	    ${RANLIB} ${LIBTCLX.A} ;\
	else \
	    exit 0 ;\
	fi
	touch made.tmp

#------------------------------------------------------------------------------

clean:
	-rm -f made.tmp
	-rm -f *.o

#------------------------------------------------------------------------------
# Restore to the distributed state.

distclean: clean
	rm -f Makefile
