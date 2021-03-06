# Generated automatically from Makefile.in by configure.
#
# Makefile --
#
# Makefile for Extended Tcl package library.  Also generates help files
# for both standard and Extended Tcl.  This assumes that Tcl has compiled
# successfully, as it is run here.
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
# $Id: Makefile,v 1.2 1994/07/05 14:13:41 ache Exp $
#------------------------------------------------------------------------------
#
SHELL = /bin/sh

#------------------------------------------------------------------------------
# Autoconfig defines that can be overridden in Config.mk

srcdir            = .
srcbasedir        = /usr/ports/lang/tclX
bldbasedir        = /usr/ports/lang/tclX
VPATH             = .
prefix            = /usr/local
exec_prefix       = /usr/local

#------------------------------------------------------------------------------
# Include user-editable defines.

.include "${bldbasedir}/Config.mk"

#------------------------------------------------------------------------------

TCL.TLIB       = ${bldbasedir}/tclmaster/tcl.tlib
TCL.TNDX       = ${bldbasedir}/tclmaster/tcl.tndx
TCLINIT.TCL    = ${bldbasedir}/tclmaster/TclInit.tcl
BUILDIDX.TCL   = ${bldbasedir}/tclmaster/buildidx.tcl
LOADOUSTER.TCL = ${bldbasedir}/tclmaster/loadouster.tcl
HELP           = ${bldbasedir}/tclmaster/help
HELPSRC        = ${srcbasedir}/tclhelp
TCLMANPAGES    = ${srcbasedir}/tools/tclmanpages
BLDMANHELP     = ../runtcl ${srcbasedir}/tools/bldmanhelp

#------------------------------------------------------------------------------

TCL_SRCS = arrayprocs.tcl   compat.tcl       convlib.tcl      edprocs.tcl    \
           forfile.tcl      globrecur.tcl    help.tcl         profrep.tcl    \
           pushd.tcl        setfuncs.tcl     showproc.tcl     stringfile.tcl \
           tcllib.tcl       fmath.tcl        tclshell.tcl     buildhelp.tcl  \
           parray.tcl       ${TCL_UCB_SRC}/library/parray.tcl

#------------------------------------------------------------------------------

all: ${TCLINIT.TCL} ${BUILDHELP.TCL} ${BUILDIDX.TCL} ${LOADOUSTER.TCL} \
     ${TCL.TLIB} HELPCP

${TCLINIT.TCL}: TclInit.tcl
	-rm -f ${TCLINIT.TCL} 
	cp ${srcdir}/TclInit.tcl ${TCLINIT.TCL}

${BUILDIDX.TCL}: buildidx.tcl
	-rm -f ${BUILDIDX.TCL}
	cp ${srcdir}/buildidx.tcl ${BUILDIDX.TCL}

${LOADOUSTER.TCL}: loadouster.tcl
	-rm -f ${LOADOUSTER.TCL}
	cp ${srcdir}/loadouster.tcl ${LOADOUSTER.TCL}

${TCL.TLIB}: ${TCL_SRCS} ${BUILDIDX.TCL} ${TCLINIT.TCL}
	-rm -f ${TCL.TLIB} ${TCL.TNDX}
	(cd ${srcdir}; cat ${TCL_SRCS}) | \
	    egrep -v '^#$$|^#  *|^#-|^#=|^  *#' > $@
	../runtcl -c "buildpackageindex ${TCL.TLIB}"

#------------------------------------------------------------------------------
# Copy help files to the master directory.
#

HELPCP: ${HELP}/Tcl.brf ${HELP}/TclX.brf

${HELP}/Tcl.brf ${HELP}/TclX.brf: ${HELPSRC}/Tcl.brf ${HELPSRC}/TclX.brf
	${bldbasedir}/tools/cphelpdir ${HELPSRC} ${HELP}

#------------------------------------------------------------------------------
# Build help for Tcl & TclX.

buildtclhelp: ${TCLINIT.TCL} ${TCL.TLIB}
	rm -rf ${HELPSRC}
	mkdir ${HELPSRC}
	${BLDMANHELP} ${TCL_UCB_SRC}/doc ${TCLMANPAGES} ${HELPSRC} Tcl.brf
	../runtcl -c "buildhelp ${HELPSRC} TclX.brf ${srcbasedir}/man/TclX.n"
	rm -rf ${HELP}	

#------------------------------------------------------------------------------
# Nothing to clean here, tclmaster is cleaned at the top level.
clean:

#------------------------------------------------------------------------------
# Restore to the distributed state.

distclean: clean
	rm -f Makefile
