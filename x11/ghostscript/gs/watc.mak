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

# makefile for Ghostscript, MS-DOS/Watcom C386 platform.

# ------------------------------- Options ------------------------------- #

###### This section is the only part of the file you should need to edit.

# ------ Generic options ------ #

# Define the default directory/ies for the runtime
# initialization and font files.  Separate multiple directories with \;.
# Use / to indicate directories, not a single \.

GS_LIB_DEFAULT=c:/gs\;c:/gs/fonts

# Define the name of the Ghostscript initialization file.
# (There is no reason to change this.)

GS_INIT=gs_init.ps

# Choose generic configuration options.

# Setting DEBUG=1 includes debugging features (-Z switch) in the code.
# Code runs substantially slower even if no debugging switches are set,
# and also takes about another 25K of memory.

DEBUG=0

# Setting TDEBUG=1 includes symbol table information for the Watcom debugger.
# (This option is NOT needed for using the Watcom profiler.)
# No execution time or space penalty, just larger .OBJ and .EXE files.

TDEBUG=0

# Setting NOPRIVATE=1 makes private (static) procedures and variables public,
# so they are visible to the debugger and profiler.
# No execution time or space penalty, just larger .OBJ and .EXE files.

NOPRIVATE=0

# Define the name of the executable file.

GS=gs386

# ------ Platform-specific options ------ #

# Define the drive, directory, and compiler name for the Watcom C files.
# $(%WATCOM) means use the WATCOM environment variable, which is also used
#   by the Watcom C programs themselves.
# COMP is the full compiler path name (normally $(%WATCOM)\bin\wcc386p).
# LINK is the full linker path name (normally $(%WATCOM)\bin\wlinkp).
# CLINK is the compile-and-link utility full path name (normally
#   $(%WATCOM)\binb\wcl386).
# STUB is the full path name for the DOS extender stub (normally
#   $(%WATCOM)\binb\wstub.exe).
# INCDIR contains the include files (normally $(%WATCOM)\h).
# LIBDIR contains the library files (normally $(%WATCOM)\lib386).
# Note that INCDIR and LIBDIR are always followed by a \,
#   so if you want to use the current directory, use an explicit '.'.

COMP=$(%WATCOM)\bin\wcc386p
LINK=$(%WATCOM)\bin\wlinkp
CLINK=$(%WATCOM)\binb\wcl386
STUB=$(%WATCOM)\binb\wstub.exe
INCDIR=$(%WATCOM)\h
LIBDIR=$(%WATCOM)\lib386

# Choose platform-specific options.

# Define the processor (CPU) type.  Options are 386 or 486.
# Currently the only difference is that 486 always uses in-line
# floating point.

CPU_TYPE=386

# Define the math coprocessor (FPU) type.
# Options are -1 (optimize for no FPU), 0 (optimize for FPU present,
# but do not require a FPU), 87, 287, or 387.
# If the CPU type is 486, the FPU type is irrelevant, since the 80486
# CPU includes the equivalent of an 80387 on-chip.
# An xx87 option means that the executable will run only if a FPU
# of that type (or higher) is available: this is NOT currently checked
# at runtime.

FPU_TYPE=0

# ---------------------------- End of options ---------------------------- #

# Define the platform name.

PLATFORM=watc_

# Define the name of the makefile -- used in dependencies.

MAKEFILE=watc.mak

# Define additional platform compilation flags.

PLATOPT=

!include wccommon.mak

# ------ Devices and features ------ #

# Choose the language feature(s) to include.  See gs.mak for details.
# Since we have a large address space, we include the optional features.

FEATURE_DEVS=filter.dev dps.dev level2.dev

# Choose the device(s) to include.  See devs.mak for details.

DEVICE_DEVS=vga.dev ega.dev
DEVICE_DEVS2=atiw.dev s3vga.dev tseng.dev tvga.dev
DEVICE_DEVS3=deskjet.dev djet500.dev laserjet.dev ljetplus.dev ljet2p.dev ljet3.dev
DEVICE_DEVS4=cdeskjet.dev cdjcolor.dev cdjmono.dev cdj550.dev paintjet.dev pjetxl.dev
DEVICE_DEVS5=epson.dev eps9high.dev ibmpro.dev bj10e.dev
DEVICE_DEVS8=gifmono.dev gif8.dev pcxmono.dev pcx16.dev pcx256.dev bit.dev
!include gs.mak
!include devs.mak

# -------------------------------- Library -------------------------------- #

# The Watcom C platform

watc__=gp_iwatc.$(OBJ) gp_dosfb.$(OBJ) gp_msdos.$(OBJ)
watc_.dev: $(watc__)
	$(SHP)gssetmod watc_ $(watc__)

gp_iwatc.$(OBJ): gp_iwatc.c $(string__h) $(gx_h) $(gp_h)

gp_dosfb.$(OBJ): gp_dosfb.c $(memory__h) $(gx_h) $(gp_h) $(gserrors_h) $(gxdevice_h)

gp_msdos.$(OBJ): gp_msdos.c $(dos__h) $(string__h) $(gx_h) $(gp_h)

# ----------------------------- Main program ------------------------------ #

BEGINFILES=*.err
# The Watcom compiler doesn't recognize wildcards;
# we don't want any compilation to fail.
CCBEGIN=for %%f in (gs*.c gx*.c z*.c) do $(CCC) %%f

LIBDOS=$(LIBGS) gp_iwatc.$(OBJ) gp_dosfb.$(OBJ) gp_msdos.$(OBJ) objw.tr

# Interpreter main program

GS_ALL=gs.$(OBJ) $(INT) $(INTASM) gsmain.$(OBJ) $(LIBDOS)

objwl.tr: $(MAKEFILE)
	echo SYSTEM DOS4G >objwl.tr
	echo OPTION STUB=$(STUB) >>objwl.tr
	echo OPTION STACK=8k >>objwl.tr

$(GS)$(XE): $(GS_ALL) $(ALL_DEVS) objwl.tr
	$(LINK) $(LCT) NAME $(GS) OPTION MAP=$(GS) @gsw.tr @objw.tr @objwl.tr
