#    Copyright (C) 1991, 1992, 1993 Aladdin Enterprises.  All rights reserved.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# wccommon.mak
# Section of Watcom C/C++ makefile for Ghostscript common to
# MS-DOS and MS Windows.

# This file is used by watc.mak and watcwin.mak.
# Those files supply the following parameters:
#   Configuration, public:
#	GS_LIB_DEFAULT, GS_INIT, FEATURE_DEVS, DEVICE_DEVS*
#   Configuration, internal, generic:
#	PLATFORM, MAKEFILE, AK, CC*, DEBUG, NOPRIVATE
#   Configuration, internal, specific to DOS/Windows:
#	TDEBUG, USE_ASM, ASM,
#	COMPDIR, INCDIR, LIBDIR,
#	CPU_TYPE, FPU_TYPE

# We want Unix-compatible behavior.  This is part of it.

.NOCHECK

# Define additional extensions to keep `make' happy

.EXTENSIONS: .be .z

# Define the ANSI-to-K&R dependency.  Watcom C accepts ANSI syntax.

AK=

# Define the extensions for the object and executable files.

OBJ=obj
XE=.exe

# Define the current directory prefix, shell quote string, and shell name.

EXPP=dos4gw
QQ="
SH=
# The following is needed to work around a problem in wmake.
SHP=command /c

# Define the generic compilation flags.

!ifeq CPU_TYPE 486
FPFLAGS=-fpi87
!else
!ifeq FPU_TYPE 387
FPFLAGS=-fpi87
!else
!ifeq FPU_TYPE 287
FPFLAGS=-fpi287
!else
!ifeq FPU_TYPE -1
FPFLAGS=-fpc
!else
FPFLAGS=-fpi
!endif
!endif
!endif
!endif

INTASM=
PCFBASM=

# Define the generic compilation rules.

.asm.obj:
	$(ASM) $(ASMFLAGS) $<;

# Make sure we get the right default target for make.

dosdefault: $(GS)$(XE)
	%null

# -------------------------- Auxiliary programs --------------------------- #

echogs$(XE): echogs.c
	echo OPTION STUB=$(STUB) >_temp_.tr
	$(CCL) $(CCFLAGS) -i=$(LIBDIR) @_temp_.tr echogs.c

genarch$(XE): genarch.c
	$(CCL) $(CCFLAGS) -i=$(LIBDIR) genarch.c

genconf$(XE): genconf.c
	echo OPTION STUB=$(STUB) >_temp_.tr
	$(CCL) $(CCFLAGS) -i=$(LIBDIR) @_temp_.tr genconf.c

# Define the compilation flags.

!ifneq NOPRIVATE 0
CP=-dNOPRIVATE
!else
CP=
!endif

!ifneq DEBUG 0
CD=-dDEBUG
!else
CD=
!endif

!ifneq TDEBUG 0
CT=-d2
LCT=DEBUG ALL
!else
CT=-d1
LCT=DEBUG LINES
!endif

!ifneq DEBUG 0
CS=
!else
CS=-s
!endif

GENOPT=$(CP) $(CD) $(CT) $(CS)

CCFLAGS=$(GENOPT) $(PLATOPT) $(FPFLAGS)
CC=$(COMP) -oi -i=$(INCDIR) $(CCFLAGS) -zq
CCL=$(CLINK) -p -oi -i=$(INCDIR) -l=dos4g
CCC=$(CC)
CCD=$(CC)
CCCF=$(CC)
CCINT=$(COMP) -oit -i=$(INCDIR) $(CCFLAGS)

.c.obj:
	$(CCC) $<
