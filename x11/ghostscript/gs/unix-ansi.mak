#    Copyright (C) 1989, 1990, 1991, 1993 Aladdin Enterprises.  All rights reserved.
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

# makefile for Ghostscript, Unix/ANSI C/X11 configuration.

# ------------------------------- Options ------------------------------- #

####### The following are the only parts of the file you should need to edit.

# ------ Generic options ------ #

# Define the installation commands and target directories for
# executables and files.  Only relevant to `make install'.

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL) -m 775
INSTALL_DATA = $(INSTALL) -m 664

prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
datadir = $(prefix)/lib
gsdatadir = $(datadir)/ghostscript

# Define the default directory/ies for the runtime
# initialization and font files.  Separate multiple directories with a :.

GS_LIB_DEFAULT=$(gsdatadir):$(gsdatadir)/fonts

# Define the name of the Ghostscript initialization file.
# (There is no reason to change this.)

GS_INIT=gs_init.ps

# Choose generic configuration options.

# -DDEBUG
#	includes debugging features (-Z switch) in the code.
#	  Code runs substantially slower even if no debugging switches
#	  are set.
# -DNOPRIVATE
#	makes private (static) procedures and variables public,
#	  so they are visible to the debugger and profiler.
#	  No execution time or space penalty.

GENOPT=

# Define the name of the executable file.

GS=gs

# ------ Platform-specific options ------ #

# Define the name of the C compiler.  If the standard compiler for your
# platform is ANSI-compatible, leave this line commented out; if not,
# uncomment the line and insert the proper definition.

#CC=some_C_compiler

# Define the other compilation flags.
# Add -DBSD4_2 for 4.2bsd systems.
# Add -DSYSV for System V or DG/UX.
# Add -DSVR4 (not -DSYSV) for System V release 4.
# The HP 400 seems to want -Aa -w -D_HPUX_SOURCE.
# XCFLAGS can be set from the command line.

CFLAGS=-O $(XCFLAGS)

# Define platform flags for ld.
# SunOS and some others want -X; Ultrix wants -x.
# SunOS 4.n may need -Bstatic.
# Apollos running DomainOS don't support -X (and -x has no effect).
# XLDFLAGS can be set from the command line.

LDFLAGS=$(XLDFLAGS)

# Define any extra libraries to link into the executable.
# ISC Unix 2.2 wants -linet.
# SCO Unix needs -lsocket if you aren't including the X11 driver.
# (Libraries required by individual drivers are handled automatically.)

EXTRALIBS=

# Define the include switch(es) for the X11 header files.
# This can be null if handled in some other way (e.g., the files are
# in /usr/include, or the directory is supplied by an environment variable).
# Note that x_.h expects to find the header files in $(XINCLUDE)/X11,
# not in $(XINCLUDE).

XINCLUDE=-I/usr/local/X/include

# Define the directory/ies for the X11 library files.
# This can be null if these files are in the default linker search path.

XLIBDIRS=-L/usr/local/X/lib

# ------ Devices and features ------ #

# Choose the language feature(s) to include.  See gs.mak for details.

FEATURE_DEVS=filter.dev dps.dev level2.dev

# Choose the device(s) to include.  See devs.mak for details.

DEVICE_DEVS=x11.dev
DEVICE_DEVS9=pbm.dev pbmraw.dev pgm.dev pgmraw.dev ppm.dev ppmraw.dev bit.dev

# ---------------------------- End of options --------------------------- #

# Define the name of the makefile -- used in dependencies.

MAKEFILE=unix-ansi.mak

# Define the ANSI-to-K&R dependency (none for ANSI compilers).

AK=

# Define the compilation rules and flags.

CCC=$(CC) $(CCFLAGS) -c

# --------------------------- Generic makefile ---------------------------- #

# The remainder of the makefile (unixhead.mak, gs.mak, devs.mak, unixtail.mak)
# is generic.  tar_cat concatenates all these together.
#    Copyright (C) 1990, 1991, 1993 Aladdin Enterprises.  All rights reserved.
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

# Partial makefile for Ghostscript, common to all Unix configurations.

# This part of the makefile gets inserted after the compiler-specific part
# (xxx-head.mak) and before gs.mak and devs.mak.

# ----------------------------- Generic stuff ----------------------------- #

# Define the platform name.  For a "stock" System V platform,
# use sysv_ instead of unix_.

PLATFORM=unix_

# Define the extensions for the object and executable files.

OBJ=o
XE=

# Define the current directory prefix, shell quote string, and shell names.

EXP=./
QQ=\"
SHELL=/bin/sh
SH=$(SHELL)
SHP=$(SH) $(EXP)

# Define the compilation rules and flags.

CCFLAGS=$(GENOPT) $(CFLAGS)

.c.o: $(AK)
	$(CCC) $*.c

CCCF=$(CCC)
CCD=$(CCC)
CCINT=$(CCC)
#    Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.
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

# Generic makefile for Ghostscript.
# The platform-specific makefiles `include' this file.
# They define the following symbols:
#	GS - the name of the executable (without the extension, if any).
#	GS_LIB_DEFAULT - the default directory/ies for searching for the
#		initialization and font files at run time.
#	DEVICE_DEVS - the devices to include in the executable.
#		See devs.mak for details.
#	DEVICE_DEVS1...DEVICE_DEVS9 - additional devices, if the definition of
#		DEVICE_DEVS doesn't fit on one line.
#		See devs.mak for details.
#	FEATURE_DEVS - the optional features to include in the
#		executable.  Current features are:
#		    dps - (partial) support for Display PostScript extensions:
#			see language.doc for details.
#		    level2 - (partial) support for PostScript Level 2
#			extensions: see language.doc for details.
#		    compfont - support for composite (type 0) fonts.
#			*** NOT IMPLEMENTED YET. ***
#		    filter - support for Level 2 filters (other than eexec,
#			ASCIIHexEncode/Decode, NullEncode, PFBDecode,
#			and SubFileDecode, which are always included).
#		    ccfonts - precompile fonts into C, and link them
#			with the executable.  In the standard makefiles,
#			this is only implemented for a very few fonts:
#			see fonts.doc for details.
# It is very unlikely that anyone would want to edit the remaining
#   symbols, but we describe them here for completeness:
#	GS_INIT - the name of the initialization file for Ghostscript,
#		normally gs_init.ps.
#	PLATFORM - a "device" name for the platform, so that platforms can
#		add various kinds of resources like devices and features.
#	QQ - a " preceded by whatever escape characters are needed to
#		persuade the shell to pass a " to a program (" on MS-DOS,
#		\" on Unix).
#	XE - the extension for executable files (e.g., null or .exe).
#	OBJ - the extension for relocatable object files (e.g., o or obj).
#	BEGINFILES - the list of files that `make begin' should delete.
#	CCBEGIN - the compilation command for `make begin', normally
#		$(CCC) *.c.
#	CCC - the C invocation for normal compilation.
#	CCD - the C invocation for files that store into frame buffers or
#		device registers.  Needed because some optimizing compilers
#		will eliminate necessary stores.
#	CCCF - the C invocation for compiled fonts and other large,
#		self-contained data modules.  Needed because MS-DOS
#		requires using the 'huge' memory model for these.
#	CCINT - the C invocation for compiling the main interpreter module,
#		normally the same as CCC: this is needed because the
#		Borland compiler generates *worse* code for this module
#		(but only this module) when optimization (-O) is turned on.
#	AK - if source files must be converted from ANSI to K&R syntax,
#		this is ansi2knr$(XE); if not, it is null.
#		If a particular platform requires other utility programs
#		to be built, AK must include them too.
#	SHP - the prefix for invoking a shell script in the current directory
#		(null for MS-DOS, $(SH) ./ for Unix).
#	EXPP, EXP - the prefix for invoking an executable program in the
#		current directory (null for MS-DOS, ./ for Unix).
#	SH - the shell for scripts (null on MS-DOS, sh on Unix).
# The platform-specific makefiles must also include rules for creating
#   ansi2knr$(XE), genarch$(XE), and genconf$(XE) from the corresponding
#   .c files -- this is needed because Turbo C and Unix C treat the -o
#   switch slightly differently (Turbo C requires no following space,
#   Unix C requires a following space), and I haven't found a way to capture
#   the difference in a macro.

all default: $(GS)$(XE)

distclean realclean: clean
	rm -f makefile

clean mostlyclean:
	rm -f *.$(OBJ) *.a core gmon.out
	rm -f *.dev *.d_* arch.h gconfig*.h o*.tr l*.tr
	rm -f t _temp_* _temp_*.* *.map *.sym
	rm -f ansi2knr$(XE) echogs$(XE) genarch$(XE) genconf$(XE)
	rm -f $(GS)$(XE) $(BEGINFILES)

# A rule to do a quick and dirty compilation attempt when first installing
# Ghostscript.  Many of the compilations will fail: follow this with 'make'.

begin:
	rm -f arch.h genarch$(XE) $(GS)$(XE) $(BEGINFILES)
	make arch.h
	- $(CCBEGIN)
	rm -f gconfig.$(OBJ) gdev*.$(OBJ) gp_*.$(OBJ) gsmisc.$(OBJ)
	rm -f iccfont.$(OBJ) iinit.$(OBJ) interp.$(OBJ) zfiledev.$(OBJ)

# Auxiliary programs

arch.h: genarch$(XE)
	$(EXPP) $(EXP)genarch arch.h

# -------------------------------- Library -------------------------------- #

# Define the inter-dependencies of the .h files.
# Since not all versions of `make' defer expansion of macros,
# we must list these in bottom-to-top order.

# Generic files

arch_h=arch.h
std_h=std.h $(arch_h)

# Platform interfaces

gp_h=gp.h
gpcheck_h=gpcheck.h

# C library interfaces

# Because of variations in the "standard" header files between systems, and
# because we must include std.h before any file that includes sys/types.h,
# we define local include files named *_.h to substitute for <*.h>.

vmsmath_h=vmsmath.h

dos__h=dos_.h
ctype__h=ctype_.h $(std_h)
errno__h=errno_.h
malloc__h=malloc_.h $(std_h)
math__h=math_.h $(std_h) $(vmsmath_h)
memory__h=memory_.h $(std_h)
stat__h=stat_.h $(std_h)
stdio__h=stdio_.h $(std_h)
string__h=string_.h $(std_h)
time__h=time_.h $(std_h)
windows__h=windows_.h

# Miscellaneous

gdebug_h=gdebug.h
gsio_h=gsio.h
gstypes_h=gstypes.h
gs_h=gs.h $(stdio__h) $(gsio_h) $(gstypes_h)
gx_h=gx.h $(gs_h) $(gdebug_h)
gconfig_h=gconfig.h gsconfig.h
gserrors_h=gserrors.h

GX=$(AK) $(gx_h)
GXERR=$(GX) $(gserrors_h)

###### Low-level facilities and utilities

### Include files

gsccode_h=gsccode.h
gschar_h=gschar.h $(gsccode_h)
gscie_h=gscie.h
gscolor_h=gscolor.h
gscolor2_h=gscolor2.h
gscoord_h=gscoord.h
gscrypt1_h=gscrypt1.h
gscspace_h=gscspace.h
gsfont_h=gsfont.h
gsimage_h=gsimage.h
gsmatrix_h=gsmatrix.h
gspaint_h=gspaint.h
gspath_h=gspath.h
gsprops_h=gsprops.h
gsstate_h=gsstate.h $(gscolor_h)
gstype1_h=gstype1.h
gsuid_h=gsuid.h
gsutil_h=gsutil.h
gsxfont_h=gsxfont.h

gxarith_h=gxarith.h
gxbitmap_h=gxbitmap.h
gxcache_h=gxcache.h $(gsuid_h) $(gsxfont_h)
gxcdir_h=gxcdir.h
gxchar_h=gxchar.h $(gschar_h)
gxclist_h=gxclist.h
# gxcldev is out of order because it include gxclist.
gxcldev_h=gxcldev.h $(gxclist_h)
gxcpath_h=gxcpath.h
gxdevice_h=gxdevice.h $(gsmatrix_h) $(gsxfont_h) $(gxbitmap_h)
gxdevmem_h=gxdevmem.h
gxfdir_h=gxfdir.h $(gxcdir_h)
gxfixed_h=gxfixed.h
gxfont_h=gxfont.h $(gsfont_h) $(gsuid_h)
gxfrac_h=gxfrac.h
gximage_h=gximage.h $(gscspace_h) $(gsimage_h)
gxlum_h=gxlum.h
gxmatrix_h=gxmatrix.h $(gsmatrix_h)
gxop1_h=gxop1.h
gxpath_h=gxpath.h
gxrefct_h=gxrefct.h
gxtype1_h=gxtype1.h $(gscrypt1_h) $(gstype1_h)
gxxfont_h=gxxfont.h $(gsccode_h) $(gsmatrix_h) $(gsuid_h) $(gsxfont_h)
# gxcolor and gxfmap are out of order because they include other files.
gxcolor_h=gxcolor.h $(gxfrac_h) $(gsuid_h)
gxfmap_h=gxfmap.h $(gxfrac_h) $(gxrefct_h)

gzcolor_h=gzcolor.h $(gscolor_h) $(gxfmap_h) $(gxlum_h)
gzdevice_h=gzdevice.h $(gxdevice_h)
gzht_h=gzht.h
gzline_h=gzline.h
gzpath_h=gzpath.h $(gxpath_h)
gzstate_h=gzstate.h $(gsstate_h) $(gxfixed_h) $(gxmatrix_h)

### Executable code

gp_nofb.$(OBJ): gp_nofb.c $(AK) \
  $(gx_h) $(gp_h) $(gxdevice_h)

gsutil.$(OBJ): gsutil.c $(AK) \
  $(std_h) $(gsprops_h) $(gsutil_h)

gxccache.$(OBJ): gxccache.c $(GXERR) $(gpcheck_h) \
  $(gxfixed_h) $(gxmatrix_h) $(gzdevice_h) $(gzcolor_h) \
  $(gxcpath_h) $(gxdevmem_h) $(gxfont_h) $(gxfdir_h) $(gxchar_h) \
  $(gxcache_h) $(gxxfont_h) $(gzstate_h) $(gzpath_h) \
  $(gscspace_h) $(gsimage_h)

gxccman.$(OBJ): gxccman.c $(GXERR) $(gpcheck_h) \
  $(gxfixed_h) $(gxmatrix_h) $(gzdevice_h) $(gzcolor_h) \
  $(gxcpath_h) $(gxdevmem_h) $(gxfont_h) $(gxfdir_h) $(gxchar_h) \
  $(gxcache_h) $(gxxfont_h) $(gzstate_h) $(gzpath_h)

gxclist.$(OBJ): gxclist.c $(GXERR) $(gpcheck_h) \
  $(gsmatrix_h) $(gxbitmap_h) $(gxcldev_h) $(gxdevice_h) $(gxdevmem_h)

gxclread.$(OBJ): gxclread.c $(GXERR) $(gpcheck_h) \
  $(gsmatrix_h) $(gxbitmap_h) $(gxcldev_h) $(gxdevice_h) $(gxdevmem_h)

gxcmap.$(OBJ): gxcmap.c $(GXERR) \
  $(gscspace_h) \
  $(gxcolor_h) $(gxdevice_h) $(gxfrac_h) $(gxlum_h) \
  $(gzcolor_h) $(gzstate_h)

gxcpath.$(OBJ): gxcpath.c $(GXERR) \
  $(gxdevice_h) $(gxfixed_h) $(gzcolor_h) $(gzpath_h) $(gxcpath_h)

gxdither.$(OBJ): gxdither.c $(GX) \
  $(gxfixed_h) $(gxlum_h) $(gxmatrix_h) $(gzstate_h) $(gzdevice_h) $(gzcolor_h) $(gzht_h)

gxdraw.$(OBJ): gxdraw.c $(GXERR) $(gpcheck_h) \
  $(gxfixed_h) $(gxmatrix_h) $(gxbitmap_h) $(gzcolor_h) $(gzdevice_h) $(gzstate_h)

gxfill.$(OBJ): gxfill.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxdevice_h) $(gzcolor_h) $(gzpath_h) $(gzstate_h) $(gxcpath_h)

gxhint1.$(OBJ): gxhint1.c $(GXERR) \
  $(gxarith_h) $(gxfixed_h) $(gxmatrix_h) $(gxdevmem_h) $(gxchar_h) $(gxfont_h) $(gxtype1_h) \
  $(gzdevice_h) $(gzstate_h)

gxhint2.$(OBJ): gxhint2.c $(GXERR) \
  $(gxarith_h) $(gxfixed_h) $(gxmatrix_h) $(gxdevmem_h) $(gxchar_h) $(gxfont_h) $(gxtype1_h) $(gxop1_h) \
  $(gzdevice_h) $(gzstate_h)

gxht.$(OBJ): gxht.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxbitmap_h) $(gzstate_h) $(gzcolor_h) $(gzdevice_h) $(gzht_h)

gxpath.$(OBJ): gxpath.c $(GXERR) \
  $(gxfixed_h) $(gzpath_h)

gxpath2.$(OBJ): gxpath2.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gzpath_h)

gxpcopy.$(OBJ): gxpcopy.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gzpath_h)

gxstroke.$(OBJ): gxstroke.c $(GXERR) $(gpcheck_h) \
  $(gscoord_h) $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) \
  $(gzstate_h) $(gzcolor_h) $(gzdevice_h) $(gzline_h) $(gzpath_h)

###### High-level facilities

gschar.$(OBJ): gschar.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) $(gzdevice_h) $(gxdevmem_h) $(gxfont_h) $(gxchar_h) $(gxcache_h) $(gstype1_h) $(gspath_h) $(gzpath_h) $(gzcolor_h) $(gzstate_h)

gscolor.$(OBJ): gscolor.c $(GXERR) \
  $(gscspace_h) $(gxcolor_h) $(gxdevice_h) $(gxrefct_h) \
  $(gzstate_h) $(gzcolor_h)

gscoord.$(OBJ): gscoord.c $(GXERR) \
  $(gsccode_h) $(gxarith_h) $(gxfixed_h) $(gxfont_h) $(gxmatrix_h) \
  $(gzdevice_h) $(gzstate_h) $(gscoord_h)

gsdevice.$(OBJ): gsdevice.c $(GXERR) \
  $(gxarith_h) $(gsprops_h) $(gsutil_h) $(gxbitmap_h) $(gxdevmem_h) \
  $(gzstate_h) $(gzdevice_h)

gsfile.$(OBJ): gsfile.c $(GXERR) \
  $(gsmatrix_h) $(gxdevice_h) $(gxdevmem_h)

gsfont.$(OBJ): gsfont.c $(GXERR) \
  $(gxdevice_h) $(gxfixed_h) $(gxmatrix_h) $(gxfont_h) $(gxfdir_h) \
  $(gzstate_h)

gsht.$(OBJ): gsht.c $(GXERR) \
  $(gzht_h) $(gzstate_h)

gsimage.$(OBJ): gsimage.c $(GXERR) $(gpcheck_h) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) $(gspaint_h) \
  $(gzcolor_h) $(gzdevice_h) $(gzpath_h) $(gzstate_h) \
  $(gxcolor_h) $(gxcpath_h) $(gxdevmem_h) $(gximage_h)

gsimage1.$(OBJ): gsimage1.c $(GXERR) $(gpcheck_h) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) $(gscspace_h) $(gspaint_h) \
  $(gzcolor_h) $(gzdevice_h) $(gzpath_h) $(gzstate_h) \
  $(gxcolor_h) $(gxcpath_h) $(gxdevmem_h) $(gximage_h)

gsimage2.$(OBJ): gsimage2.c $(GXERR) $(gpcheck_h) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h) $(gscspace_h) $(gspaint_h) \
  $(gzcolor_h) $(gzdevice_h) $(gzpath_h) $(gzstate_h) \
  $(gxcolor_h) $(gxcpath_h) $(gxdevmem_h) $(gximage_h)

gsimpath.$(OBJ): gsimpath.c $(GXERR) \
  $(gsmatrix_h) $(gsstate_h) $(gspath_h)

gsline.$(OBJ): gsline.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gzstate_h) $(gzline_h)

gsmatrix.$(OBJ): gsmatrix.c $(GXERR) \
  $(gxfixed_h) $(gxarith_h) $(gxmatrix_h)

gsmisc.$(OBJ): gsmisc.c $(GX) $(errno__h) $(malloc__h) $(memory__h) $(MAKEFILE)
	$(CCC) -DUSE_ASM=0$(USE_ASM) gsmisc.c

gspaint.$(OBJ): gspaint.c $(GXERR) $(gpcheck_h) \
  $(gxfixed_h) $(gxmatrix_h) $(gspaint_h) $(gzpath_h) $(gzstate_h) $(gzdevice_h) $(gxcpath_h) $(gxdevmem_h) $(gximage_h)

gspath.$(OBJ): gspath.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gxpath_h) $(gzstate_h)

gspath2.$(OBJ): gspath2.c $(GXERR) \
  $(gspath_h) $(gxfixed_h) $(gxmatrix_h) $(gzstate_h) $(gzpath_h) $(gzdevice_h)

gsstate.$(OBJ): gsstate.c $(GXERR) \
  $(gscie_h) $(gscolor2_h) $(gscspace_h) $(gxcolor_h) $(gxrefct_h) \
  $(gzstate_h) $(gzcolor_h) $(gzdevice_h) $(gzht_h) $(gzline_h) $(gzpath_h)

gstdev.$(OBJ): gstdev.c $(GXERR) \
  $(gxbitmap_h) $(gxdevice_h) $(gxfixed_h) $(gxmatrix_h)

gstype1.$(OBJ): gstype1.c $(GXERR) \
  $(gxarith_h) $(gxfixed_h) $(gxmatrix_h) $(gxchar_h) $(gxdevmem_h) $(gxop1_h) $(gxtype1_h) \
  $(gzstate_h) $(gzdevice_h) $(gzpath_h)

###### The internal devices

gdevmem_h=gdevmem.h

gdevemap.$(OBJ): gdevemap.c $(AK) $(std_h)

gdevmem1.$(OBJ): gdevmem1.c $(AK) \
  $(gx_h) $(gserrors_h) $(gxdevice_h) $(gxdevmem_h) $(gdevmem_h)

gdevmem2.$(OBJ): gdevmem2.c $(AK) \
  $(gx_h) $(gxdevice_h) $(gxdevmem_h) $(gdevmem_h)

gdevmem3.$(OBJ): gdevmem3.c $(AK) \
  $(gx_h) $(gxdevice_h) $(gxdevmem_h) $(gdevmem_h)

###### Files dependent on the installed devices, features, and platform.
# Generating gconfig.h also generates o*.tr and l*.tr.

# gconfig.h shouldn't have to depend on ALL_DEVS, but that would
# involve rewriting gsconfig to only save the device name, not the
# contents of the <device>.D_# files.

ALL_DEVS=$(FEATURE_DEVS) $(PLATFORM).dev \
  $(DEVICE_DEVS) $(DEVICE_DEVS1) \
  $(DEVICE_DEVS2) $(DEVICE_DEVS3) $(DEVICE_DEVS4) $(DEVICE_DEVS5)\
  $(DEVICE_DEVS6) $(DEVICE_DEVS7) $(DEVICE_DEVS8) $(DEVICE_DEVS9)

gconfig.h obj.tr objw.tr ld.tr lib.tr: \
  devs.mak $(MAKEFILE) echogs$(XE) genconf$(XE) $(ALL_DEVS)
	$(EXP)echogs -w t.cfg - $(FEATURE_DEVS) $(PLATFORM).dev
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS1)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS2)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS3)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS4)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS5)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS6)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS7)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS8)
	$(EXP)echogs -a t.cfg - $(DEVICE_DEVS9)
	$(EXP)genconf @t.cfg -h gconfig.h -l lib.tr -o obj.tr -u ld.tr -w objw.tr
	rm t.cfg
	$(EXP)echogs -a gconfig.h -x 23 define GS_LIB_DEFAULT -x 2022 $(GS_LIB_DEFAULT) -x 22
	$(EXP)echogs -a gconfig.h -x 23 define GS_INIT -x 2022 $(GS_INIT) -x 22

gconfig.$(OBJ): gconfig.c $(AK) $(gconfig_h) $(MAKEFILE)

###### On Unix, we pre-link all of the library except the back end.
###### On MS-DOS, we have to do the whole thing at once.

LIBGS=gschar.$(OBJ) gscolor.$(OBJ) gscoord.$(OBJ) \
 gsdevice.$(OBJ) gsfile.$(OBJ) gsfont.$(OBJ) gsht.$(OBJ) \
 gsimage.$(OBJ) gsimage1.$(OBJ) gsimage2.$(OBJ) \
 gsimpath.$(OBJ) gsline.$(OBJ) gsmatrix.$(OBJ) gsmisc.$(OBJ) \
 gspaint.$(OBJ) gspath.$(OBJ) gspath2.$(OBJ) \
 gsstate.$(OBJ) gstdev.$(OBJ) gstype1.$(OBJ) gsutil.$(OBJ) \
 gxccache.$(OBJ) gxccman.$(OBJ) gxclist.$(OBJ) gxclread.$(OBJ) \
 gxcmap.$(OBJ) gxcpath.$(OBJ) \
 gxdither.$(OBJ) gxdraw.$(OBJ) gxfill.$(OBJ) \
 gxhint1.$(OBJ) gxhint2.$(OBJ) gxht.$(OBJ) \
 gxpath.$(OBJ) gxpath2.$(OBJ) gxpcopy.$(OBJ) gxstroke.$(OBJ) \
 gdevmem1.$(OBJ) gdevmem2.$(OBJ) gdevmem3.$(OBJ) gconfig.$(OBJ)

# ------------------------------ Interpreter ------------------------------ #

###### Include files

alloc_h=alloc.h
astate_h=astate.h
ccfont_h=ccfont.h
dict_h=dict.h
dparam_h=dparam.h
dstack_h=dstack.h
errors_h=errors.h
estack_h=estack.h
filedev_h=filedev.h
files_h=files.h
font_h=font.h
ilevel_h=ilevel.h
iname_h=iname.h
iref_h=iref.h
iscan_h=iscan.h
ivmspace_h=ivmspace.h
iutil_h=iutil.h
main_h=main.h
opdef_h=opdef.h
ostack_h=ostack.h
overlay_h=overlay.h
packed_h=packed.h
save_h=save.h
scanchar_h=scanchar.h
sbits_h=sbits.h
shc_h=shc.h
state_h=state.h
store_h=store.h
stream_h=stream.h
# Nested include files
bfont_h=bfont.h $(font_h)
ghost_h=ghost.h $(gx_h) $(iref_h)
oper_h=oper.h $(gsutil_h) $(iutil_h) $(opdef_h) $(ostack_h)
scf_h=scf.h $(shc_h)
sdct_h=sdct.h $(shc_h)
# Include files for optional features
bnum_h=bnum.h
bseq_h=bseq.h
btoken_h=btoken.h

comp1_h=comp1.h $(ghost_h) $(oper_h) $(gserrors_h) $(gxfixed_h) $(gxop1_h)

gdevprn_h=gdevprn.h $(memory__h) $(string__h) $(gx_h) \
  $(gserrors_h) $(gsmatrix_h) $(gxdevice_h) $(gxdevmem_h) $(gxclist_h)

###### Utilities

GH=$(AK) $(ghost_h)

ialloc.$(OBJ): ialloc.c $(AK) $(gx_h) $(alloc_h) $(astate_h) $(ivmspace_h)

iccfont.$(OBJ): iccfont.c $(GH) gconfigf.h \
  $(ghost_h) $(alloc_h) $(ccfont_h) $(dict_h) $(dstack_h) $(errors_h) \
  $(font_h) $(iutil_h) $(iname_h) $(oper_h) $(save_h) $(store_h)

idebug.$(OBJ): idebug.c $(GH) \
  $(iutil_h) $(dict_h) $(iname_h) $(ostack_h) $(opdef_h) $(packed_h) $(store_h)

idict.$(OBJ): idict.c $(GH) \
  $(alloc_h) $(errors_h) $(ivmspace_h) $(iname_h) $(packed_h) \
  $(save_h) $(store_h) $(iutil_h) $(dict_h) $(dstack_h)

idparam.$(OBJ): idparam.c $(GH) \
  $(gsmatrix_h) $(dict_h) $(dparam_h) $(errors_h) $(iutil_h)

iinit.$(OBJ): iinit.c $(GH) $(gconfig_h) \
  $(alloc_h) $(dict_h) $(dstack_h) $(errors_h) $(ilevel_h) $(iname_h) $(oper_h) $(store_h)

iname.$(OBJ): iname.c $(GH) $(alloc_h) $(errors_h) $(ivmspace_h) $(iname_h) $(store_h)

isave.$(OBJ): isave.c $(GH) $(alloc_h) $(astate_h) $(errors_h) $(iname_h) $(packed_h) $(save_h) $(store_h)

iscan.$(OBJ): iscan.c $(GH) $(ctype__h) \
  $(alloc_h) $(dict_h) $(dstack_h) $(errors_h) \
  $(ilevel_h) $(iutil_h) $(iscan_h) $(ivmspace_h) \
  $(iname_h) $(ostack_h) $(packed_h) $(store_h) $(stream_h) $(scanchar_h)

iutil.$(OBJ): iutil.c $(GH) \
  $(errors_h) $(alloc_h) $(dict_h) $(iutil_h) $(ivmspace_h) \
  $(iname_h) $(ostack_h) $(opdef_h) $(packed_h) $(store_h) \
  $(gsmatrix_h) $(gxdevice_h) $(gzcolor_h)

sfilter.$(OBJ): sfilter.c $(AK) $(stdio__h) \
  $(scanchar_h) $(stream_h) $(gscrypt1_h)

stream.$(OBJ): stream.c $(AK) $(stdio__h) $(memory__h) \
  $(gpcheck_h) $(scanchar_h) $(stream_h)

###### Operators

OP=$(GH) $(errors_h) $(oper_h)

### Non-graphics operators

zarith.$(OBJ): zarith.c $(OP) $(store_h)

zarray.$(OBJ): zarray.c $(OP) $(alloc_h) $(packed_h) $(store_h)

zcontrol.$(OBJ): zcontrol.c $(OP) $(estack_h) $(iutil_h) $(store_h)

zdict.$(OBJ): zdict.c $(OP) $(dict_h) $(dstack_h) $(iname_h) $(store_h)

zfile.$(OBJ): zfile.c $(OP) $(gp_h) \
  $(alloc_h) $(estack_h) $(filedev_h) $(files_h) $(ilevel_h) $(iutil_h) \
  $(save_h) $(stream_h) $(store_h)

zfiledev.$(OBJ): zfiledev.c $(OP) $(string__h) $(gp_h) $(gconfig_h) \
  $(filedev_h) $(files_h) $(stream_h)

zfileio.$(OBJ): zfileio.c $(OP) $(gp_h) \
  $(estack_h) $(files_h) $(iscan_h) $(store_h) $(stream_h) \
  $(gsmatrix_h) $(gxdevice_h) $(gxdevmem_h)

zfilter.$(OBJ): zfilter.c $(OP) $(alloc_h) $(stream_h)

zgeneric.$(OBJ): zgeneric.c $(OP) \
  $(dict_h) $(estack_h) $(ivmspace_h) $(iname_h) $(packed_h) $(store_h)

zmath.$(OBJ): zmath.c $(OP) $(store_h)

zmisc.$(OBJ): zmisc.c $(OP) $(gp_h) $(errno__h) $(memory__h) $(string__h) \
  $(alloc_h) $(dict_h) $(dstack_h) $(iname_h) $(ivmspace_h) $(packed_h) $(store_h) \
  $(gscrypt1_h)

zpacked.$(OBJ): zpacked.c $(OP) \
  $(alloc_h) $(dict_h) $(ivmspace_h) $(iname_h) $(packed_h) $(save_h) $(store_h)

zprops.$(OBJ): zprops.c $(OP) \
  $(alloc_h) $(dict_h) $(iname_h) $(state_h) $(store_h) \
  $(gsprops_h) $(gsmatrix_h) $(gxdevice_h) $(gsstate_h)

zrelbit.$(OBJ): zrelbit.c $(OP) $(store_h) $(dict_h)

zstack.$(OBJ): zstack.c $(OP) $(store_h)

zstring.$(OBJ): zstring.c $(OP) \
  $(alloc_h) $(iscan_h) $(iutil_h) $(iname_h) $(store_h) $(stream_h)

ztype.$(OBJ): ztype.c $(OP) \
  $(dict_h) $(iscan_h) $(iutil_h) $(iname_h) $(stream_h) $(store_h)

zvmem.$(OBJ): zvmem.c $(OP) $(alloc_h) $(dict_h) $(dstack_h) $(estack_h) $(save_h) $(state_h) $(store_h) \
  $(gsmatrix_h) $(gsstate_h)

###### Graphics operators

zchar.$(OBJ): zchar.c $(OP) $(gxarith_h) $(gxfixed_h) $(gxmatrix_h) \
  $(gschar_h) $(gxtype1_h) $(gxdevice_h) $(gxfont_h) $(gzpath_h) $(gzstate_h) \
  $(alloc_h) $(dict_h) $(font_h) $(estack_h) $(ilevel_h) $(iname_h) $(state_h) $(store_h)

zcolor.$(OBJ): zcolor.c $(OP) $(alloc_h) $(estack_h) $(gxfixed_h) $(gxmatrix_h) $(gzstate_h) $(gxdevice_h) $(gzcolor_h) $(iutil_h) $(state_h) $(store_h)

zdevice.$(OBJ): zdevice.c $(OP) $(alloc_h) $(state_h) $(gsmatrix_h) $(gsstate_h) $(gxdevice_h) $(store_h)

zfont.$(OBJ): zfont.c $(OP) \
  $(gsmatrix_h) $(gxdevice_h) $(gxfont_h) $(gxfdir_h) $(gxcache_h) \
  $(alloc_h) $(bfont_h) $(dict_h) $(iname_h) $(packed_h) $(save_h) $(state_h) $(store_h)

zfont1.$(OBJ): zfont1.c $(OP) $(gsmatrix_h) $(gxdevice_h) $(gschar_h) $(gxfixed_h) $(gxfont_h) \
  $(bfont_h) $(dict_h) $(dparam_h) $(iname_h) $(store_h)

zfont2.$(OBJ): zfont2.c $(OP) $(gsmatrix_h) $(gxdevice_h) $(gschar_h) $(gxfixed_h) $(gxfont_h) \
  $(alloc_h) $(bfont_h) $(dict_h) $(dparam_h) $(ilevel_h) $(iname_h) \
  $(packed_h) $(save_h) $(store_h)

zgstate.$(OBJ): zgstate.c $(OP) $(alloc_h) $(gsmatrix_h) $(gsstate_h) $(state_h) $(store_h)

zht.$(OBJ): zht.c $(OP) $(alloc_h) $(estack_h) $(gsmatrix_h) $(gsstate_h) $(state_h) $(store_h)

zmatrix.$(OBJ): zmatrix.c $(OP) $(gsmatrix_h) $(state_h) $(gscoord_h) $(store_h)

zpaint.$(OBJ): zpaint.c $(OP) \
  $(alloc_h) $(estack_h) $(ilevel_h) $(state_h) $(store_h) $(stream_h) \
  $(gsimage_h) $(gsmatrix_h) $(gspaint_h)

zpath.$(OBJ): zpath.c $(OP) $(gsmatrix_h) $(gspath_h) $(state_h) $(store_h)

zpath2.$(OBJ): zpath2.c $(OP) $(alloc_h) $(estack_h) $(gspath_h) $(state_h) $(store_h)

###### Linking

INT=ialloc.$(OBJ) idebug.$(OBJ) idict.$(OBJ) idparam.$(OBJ) \
 iinit.$(OBJ) iname.$(OBJ) \
 interp.$(OBJ) isave.$(OBJ) iscan.$(OBJ) iutil.$(OBJ) \
 sfilter.$(OBJ) stream.$(OBJ) \
 zarith.$(OBJ) zarray.$(OBJ) zcontrol.$(OBJ) zdict.$(OBJ) \
 zfile.$(OBJ) zfiledev.$(OBJ) zfileio.$(OBJ) zfilter.$(OBJ) zgeneric.$(OBJ) \
 zmath.$(OBJ) zmisc.$(OBJ) zpacked.$(OBJ) zprops.$(OBJ) zrelbit.$(OBJ) \
 zstack.$(OBJ) zstring.$(OBJ) ztype.$(OBJ) zvmem.$(OBJ) \
 zchar.$(OBJ) zcolor.$(OBJ) zfont.$(OBJ) zfont1.$(OBJ) zfont2.$(OBJ) \
 zdevice.$(OBJ) zgstate.$(OBJ) zht.$(OBJ) zmatrix.$(OBJ) \
 zpaint.$(OBJ) zpath.$(OBJ) zpath2.$(OBJ)

# -------------------------- Optional features ---------------------------- #

### Additions common to Display PostScript and Level 2

# We have to split up the module list because of limitations
# on the number of arguments to a DOS batch file.
dpsand2a_=gsdps1.$(OBJ) ibnum.$(OBJ) iscan2.$(OBJ)
dpsand2b_=zbseq.$(OBJ) zchar2.$(OBJ) zdps1.$(OBJ) zupath.$(OBJ) zvmem2.$(OBJ)
dpsand2_=$(dpsand2a_) $(dpsand2b_)
dpsand2.dev: $(dpsand2_)
	$(SHP)gssetmod dpsand2 $(dpsand2a_)
	$(SHP)gsaddmod dpsand2 -obj $(dpsand2b_)
	$(SHP)gsaddmod dpsand2 -oper2 zbseq zchar2 zdps1 zupath zvmem2
	$(SHP)gsaddmod dpsand2 -ps gs_dps1

gsdps1.$(OBJ): gsdps1.c $(GXERR) $(gxfixed_h) $(gxmatrix_h) $(gzpath_h) $(gzstate_h)

ibnum.$(OBJ): ibnum.c $(GH) $(errors_h) $(stream_h) $(bnum_h) $(btoken_h)

iscan2.$(OBJ): iscan2.c $(GH) $(errors_h) \
  $(alloc_h) $(dict_h) $(dstack_h) $(iscan_h) $(iutil_h) $(ivmspace_h) \
  $(iname_h) $(ostack_h) $(save_h) $(store_h) $(stream_h) \
  $(bseq_h) $(btoken_h) $(bnum_h)

zbseq.$(OBJ): zbseq.c $(OP) $(save_h) $(store_h) $(stream_h) $(files_h) $(iname_h) $(bnum_h) $(btoken_h) $(bseq_h)

zchar2.$(OBJ): zchar2.c $(OP) $(gschar_h) $(gsmatrix_h) $(gxfixed_h) $(gxfont_h) \
  $(alloc_h) $(estack_h) $(font_h) $(iname_h) $(state_h) $(store_h) $(stream_h) $(bnum_h)

zdps1.$(OBJ): zdps1.c $(OP) $(gsmatrix_h) $(gspath_h) $(gsstate_h) \
  $(alloc_h) $(ivmspace_h) $(state_h) $(store_h) $(stream_h) $(bnum_h)

zupath.$(OBJ): zupath.c $(OP) \
  $(dict_h) $(dstack_h) $(iutil_h) $(state_h) $(store_h) $(stream_h) $(bnum_h) \
  $(gscoord_h) $(gsmatrix_h) $(gspaint_h) $(gspath_h) $(gsstate_h) \
  $(gxfixed_h) $(gxdevice_h) $(gxpath_h)

zvmem2.$(OBJ): zvmem2.c $(OP) \
  $(ivmspace_h) $(store_h)

### Display PostScript
# We should include zcontext, but it isn't in good enough shape yet:
#	$(SHP)gsaddmod dps -oper2 zcontext

dps_=
dps.dev: dpsand2.dev $(dps_)
	$(SHP)gssetmod dps $(dps_)
	$(SHP)gsaddmod dps -include dpsand2

zcontext.$(OBJ): zcontext.c $(OP) \
  $(alloc_h) $(dict_h) $(dstack_h) $(estack_h) $(state_h) $(store_h)

### Composite font support

gschar0.$(OBJ): gschar0.c $(GXERR) \
  $(gxfixed_h) $(gxmatrix_h) $(gzdevice_h) $(gxdevmem_h) $(gxfont_h) $(gxchar_h) $(gzstate_h)

zfont0.$(OBJ): zfont0.c $(OP) $(gsmatrix_h) $(gxdevice_h) $(gxfont_h) \
  $(alloc_h) $(bfont_h) $(dict_h) $(iname_h) $(state_h) $(store_h)

compfont_=zchar2.$(OBJ) zfont0.$(OBJ) gschar0.$(OBJ)
compfont.dev: $(compfont_)
	$(SHP)gssetmod compfont $(compfont_)
	$(SHP)gsaddmod compfont -oper zfont0 zchar2
	$(SHP)gsaddmod compfont -ps gs_type0

### Level 2 additions

# We have to split up the module list because of limitations
# on the number of arguments to a DOS batch file.
level2a_=gscie.$(OBJ) gscolor2.$(OBJ) zcie.$(OBJ) zcolor2.$(OBJ)
level2b_=zcspace2.$(OBJ) zht2.$(OBJ) zimage2.$(OBJ) zmisc2.$(OBJ)
level2_=$(level2a_) $(level2b_)
level2.dev: compfont.dev dpsand2.dev filter.dev $(level2_)
	$(SHP)gssetmod level2 $(level2a_)
	$(SHP)gsaddmod level2 -obj $(level2b_)
	$(SHP)gsaddmod level2 -include compfont dpsand2 filter
	$(SHP)gsaddmod level2 -oper zmisc2_level
	$(SHP)gsaddmod level2 -oper2 zcie zcolor2 zcspace2
	$(SHP)gsaddmod level2 -oper2 zht2 zimage2 zmisc2
	$(SHP)gsaddmod level2 -ps gs_lev2

gscie.$(OBJ): gscie.c $(GXERR) \
  $(gscspace_h) $(gscie_h) $(gscolor2_h) \
  $(gxarith_h) $(gxcolor_h) $(gxdevice_h) $(gxrefct_h) \
  $(gzcolor_h) $(gzstate_h)

gscolor2.$(OBJ): gscolor2.c $(GXERR) \
  $(gscie_h) $(gscolor2_h) $(gscspace_h) \
  $(gxcolor_h) $(gxdevice_h) $(gxfixed_h) $(gxmatrix_h) $(gxrefct_h) \
  $(gzcolor_h) $(gzstate_h)

zcie.$(OBJ): zcie.c $(OP) \
  $(gscspace_h) $(gscolor2_h) $(gscie_h) $(gxcolor_h) $(gxrefct_h) \
  $(alloc_h) $(dict_h) $(dparam_h) $(estack_h) $(save_h) $(state_h) $(store_h)

zcolor2.$(OBJ): zcolor2.c $(OP) \
  $(gscolor_h) $(gxcolor_h) $(gscolor2_h) $(gscspace_h) $(gsmatrix_h) \
  $(dict_h) $(dparam_h) $(iname_h) $(state_h) $(store_h)

zcspace2.$(OBJ): zcspace2.c $(OP) \
  $(gscolor_h) $(gxcolor_h) $(gscolor2_h) $(gscspace_h) $(gsmatrix_h) \
  $(dict_h) $(dparam_h) $(estack_h) $(iname_h) $(state_h) $(store_h)

zht2.$(OBJ): zht2.c $(OP) \
  $(dict_h) $(dparam_h) $(iname_h) $(state_h) $(store_h)

zimage2.$(OBJ): zimage2.c $(OP) \
  $(gscolor_h) $(gscolor2_h) $(gscspace_h) $(gsmatrix_h) $(gxcolor_h) \
  $(dict_h) $(dparam_h) $(ilevel_h) $(state_h)

zmisc2.$(OBJ): zmisc2.c $(OP) \
  $(gsfont_h) \
  $(dict_h) $(dparam_h) $(dstack_h) $(estack_h) $(ilevel_h) $(iname_h) $(store_h)

### Filters other than the ones in sfilter.c

sbits.$(OBJ): sbits.c $(AK) $(stdio__h) $(sbits_h) $(stream_h)

scftab.$(OBJ): scftab.c $(AK) $(std_h) $(scf_h)

scfdtab.$(OBJ): scfdtab.c $(AK) $(std_h) $(scf_h)

scfd.$(OBJ): scfd.c $(AK) $(stdio__h) $(gdebug_h)\
  $(sbits_h) $(scf_h) $(stream_h)

scfe.$(OBJ): scfe.c $(AK) $(stdio__h) $(gdebug_h)\
  $(sbits_h) $(scf_h) $(stream_h)

sdctd.$(OBJ): sdctd.c $(AK) $(stdio__h) $(gdebug_h) $(stream_h)

sdcte.$(OBJ): sdcte.c $(AK) $(stdio__h) $(gdebug_h) $(stream_h)

sfilter2.$(OBJ): sfilter2.c $(AK) $(stdio__h) $(scanchar_h) $(stream_h)

slzwd.$(OBJ): slzwd.c $(AK) $(stdio__h) $(gdebug_h) $(stream_h)

slzwe.$(OBJ): slzwe.c $(AK) $(stdio__h) $(gdebug_h) $(stream_h)

zfilter2.$(OBJ): zfilter2.c $(OP) $(alloc_h) $(dict_h) $(dparam_h) $(sdct_h) $(stream_h)

# Because of size limits on the DOS command line,
# we have to break this up into two parts.
filter_1=zfilter2.$(OBJ) sfilter2.$(OBJ) sbits.$(OBJ)
filter_2=scfdtab.$(OBJ) scftab.$(OBJ) scfd.$(OBJ) scfe.$(OBJ) 
filter_3=sdctd.$(OBJ) sdcte.$(OBJ) slzwd.$(OBJ) slzwe.$(OBJ)
filter.dev: $(filter_1) $(filter_2) $(filter_3)
	$(SHP)gssetmod filter $(filter_1)
	$(SHP)gsaddmod filter -obj $(filter_2)
	$(SHP)gsaddmod filter -obj $(filter_3)
	$(SHP)gsaddmod filter -oper zfilter2

### Precompiled fonts.  See fonts.doc for more information.

CCFONT=$(OP) $(ccfont_h)

# List the fonts we are going to compile.
# Because of intrinsic limitations in `make', we have to list
# the object file names and the font names separately.
ccfonts1_=uglyr.$(OBJ)
ccfonts1=uglyr

ccfonts.dev: $(MAKEFILE) gs.mak iccfont.$(OBJ) \
  $(ccfonts1_) $(ccfonts2_) $(ccfonts3_) $(ccfonts4_) $(ccfonts5_)
	$(SHP)gssetmod ccfonts iccfont.$(OBJ)
	$(SHP)gsaddmod ccfonts -obj $(ccfonts1_)
	$(SHP)gsaddmod ccfonts -obj $(ccfonts2_)
	$(SHP)gsaddmod ccfonts -obj $(ccfonts3_)
	$(SHP)gsaddmod ccfonts -obj $(ccfonts4_)
	$(SHP)gsaddmod ccfonts -obj $(ccfonts5_)
	$(SHP)gsaddmod ccfonts -oper ccfonts

gconfigf.h: $(MAKEFILE) gs.mak genconf$(XE)
	$(SHP)gssetmod ccfonts_
	$(SHP)gsaddmod ccfonts_ -font $(ccfonts1)
	$(SHP)gsaddmod ccfonts_ -font $(ccfonts2)
	$(SHP)gsaddmod ccfonts_ -font $(ccfonts3)
	$(SHP)gsaddmod ccfonts_ -font $(ccfonts4)
	$(SHP)gsaddmod ccfonts_ -font $(ccfonts5)
	$(EXP)genconf ccfonts_.dev -f gconfigf.h

uglyr.$(OBJ): uglyr.c $(CCFONT)
	$(CCCF) uglyr.c

ncrr.$(OBJ): ncrr.c $(CCFONT)
	$(CCCF) ncrr.c

pagk.$(OBJ): pagk.c $(CCFONT)
	$(CCCF) pagk.c

psyr.$(OBJ): psyr.c $(CCFONT)
	$(CCCF) psyr.c

ptmr.$(OBJ): ptmr.c $(CCFONT)
	$(CCCF) ptmr.c

pzdr.$(OBJ): pzdr.c $(CCFONT)
	$(CCCF) pzdr.c

# ----------------------------- Main program ------------------------------ #

# Interpreter main program

gs.$(OBJ): gs.c $(GH) $(ctype__h) \
  $(gxdevice_h) $(gxdevmem_h) \
  $(alloc_h) $(errors_h) $(estack_h) $(files_h) $(iscan_h) $(main_h) $(ostack_h) $(store_h) $(stream_h)

gsmain.$(OBJ): gsmain.c $(GH) \
  $(gp_h) $(gsmatrix_h) $(gxdevice_h) $(gserrors_h) \
  $(estack_h) $(files_h) $(iscan_h) $(main_h) $(ostack_h) $(store_h)

interp.$(OBJ): interp.c $(GH) \
  $(errors_h) $(estack_h) $(iname_h) $(dict_h) $(dstack_h) $(iscan_h) $(oper_h) $(ostack_h) $(packed_h) $(save_h) $(store_h) $(stream_h)
	$(CCINT) interp.c
#    Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.
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

# makefile for Ghostscript device drivers.

# -------------------------------- Catalog ------------------------------- #

# It is possible to build Ghostscript with an arbitrary collection of
# device drivers, although some drivers are supported only on a subset
# of the target platforms.  The currently available drivers are:

# Displays:
#   MS-DOS EGA and VGA:
#	ega	EGA (640x350, 16-color)
#	vga	VGA (640x480, 16-color)
#   MS-DOS SuperVGA:
# +	atiw	ATI Wonder SuperVGA, 256-color modes
# +     atiw16  ATI Wonder SuperVGA in 800x600, 16-color mode
#	s3vga	SuperVGA with S3 86C911 chip (e.g., Diamond Stealth board)
#	tseng	SuperVGA using Tseng Labs ET3000/4000 chips, 256-color modes
#	tseng16  Tseng Labs SuperVGA in 800x600, 16-color mode (256K memory)
# +	tvga	Trident SuperVGA, 256-color modes
# +	tvga16	Trident SuperVGA in 800x600, 16-color mode (256K memory)
#   ****** NOTE: The vesa device does not work with the Watcom (32-bit MS-DOS)
#   ****** compiler or executable.
#	vesa	SuperVGA with VESA standard API driver
#   MS-DOS other:
#	bgi	Borland Graphics Interface (CGA)  [MS-DOS only]
# *	herc	Hercules Graphics display   [MS-DOS only]
#	mswin	Microsoft Windows 3.0, 3.1  [MS Windows only]
#	mswinprn  Microsoft Windows 3.0, 3.1 printer  [MS Windows only]
# *	pe	Private Eye display
#   Unix and VMS:
#   ****** NOTE: For direct frame buffer addressing under SCO Unix or Xenix,
#   ****** edit the definition of EGAVGA below.
# *	att3b1	AT&T 3b1/Unixpc monochrome display   [3b1 only]
# *	sonyfb	Sony Microsystems monochrome display   [Sony only]
# *	sunview  SunView window system   [SunOS only]
#	x11	X Windows version 11, release >=4   [Unix and VMS only]
# Printers:
# *	appledmp  Apple Dot Matrix Printer (should also work with Imagewriter)
#	bj10e	Canon BubbleJet BJ10e
# *	bj200	Canon BubbleJet BJ200
# *	cdeskjet  H-P DeskJet 500C with 1 bit/pixel color
# *	cdjcolor  H-P DeskJet 500C with 24 bit/pixel color and
#		high-quality color (Floyd-Steinberg) dithering
# *	cdjmono  H-P DeskJet 500C printing black only
# *	cdj500	H-P DeskJet 500C (same as cdjcolor)
# *	cdj550	H-P DeskJet 550C
# *	declj250  alternate DEC LJ250 driver
# +	deskjet  H-P DeskJet and DeskJet Plus
# *	dfaxhigh  DigiBoard, Inc.'s DigiFAX software format (high resolution)
# *	dfaxlow  DigiFAX low (normal) resolution
#	djet500  H-P DeskJet 500
# *	djet500c  H-P DeskJet 500C
#	epson	Epson-compatible dot matrix printers (9- or 24-pin)
# +	eps9high  Epson-compatible 9-pin, interleaved lines
#		(triple resolution)
# *	epsonc	Epson LQ-2550 and Fujitsu 3400/2400/1200 color printers
# *	escp2	Epson ESC/P 2 language printers, including Stylus 800
# +     ibmpro  IBM 9-pin Proprinter
# *	jetp3852  IBM Jetprinter ink-jet color printer (Model #3852)
# +	laserjet  H-P LaserJet
# *	la50	DEC LA50 printer
# *	la75	DEC LA75 printer
# *	lbp8	Canon LBP-8II laser printer
# *	ln03	DEC LN03 printer
# *	lj250	DEC LJ250 Companion color printer
# +	ljet2p	H-P LaserJet IId/IIp/III* with TIFF compression
# +	ljet3	H-P LaserJet III* with Delta Row compression
# +	ljet4	H-P LaserJet 4 (defaults to 600 dpi)
# +	ljetplus  H-P LaserJet Plus
# *	m8510	C.Itoh M8510 printer
# *	necp6	NEC P6/P6+/P60 printers at 360 x 360 DPI resolution
# *	nwp533  Sony Microsystems NWP533 laser printer   [Sony only]
# *	oki182	Okidata MicroLine 182
#	paintjet  H-P PaintJet color printer
# *	pj	alternate PaintJet XL driver 
# *	pjxl	H-P PaintJet XL color printer
# *	pjxl300  H-P PaintJet XL300 color printer
# *	r4081	Ricoh 4081 laser printer
# *	sparc	SPARCprinter
# *	t4693d2  Tektronix 4693d color printer, 2 bits per R/G/B component
# *	t4693d4  Tektronix 4693d color printer, 4 bits per R/G/B component
# *	t4693d8  Tektronix 4693d color printer, 8 bits per R/G/B component
# *	tek4696  Tektronix 4695/4696 inkjet plotter
# *	trufax	TruFax facsimile driver  [Unix only]
# File formats and others:
#	bit	A plain "bit bucket" device
#	bmpmono	Monochrome MS Windows .BMP file format
#	bmp16	4-bit (EGA/VGA) .BMP file format
#	bmp256	8-bit (256-color) .BMP file format
#	bmp16m	24-bit .BMP file format
#	gifmono	Monochrome GIF file format
#	gif8	8-bit color GIF file format
#	pcxmono	Monochrome PCX file format
#	pcxgray	8-bit gray scale PCX file format
#	pcx16	Older color PCX file format (EGA/VGA, 16-color)
#	pcx256	Newer color PCX file format (256-color)
#	pbm	Portable Bitmap (plain format)
#	pbmraw	Portable Bitmap (raw format)
#	pgm	Portable Graymap (plain format)
#	pgmraw	Portable Graymap (raw format)
#	ppm	Portable Pixmap (plain format)
#	ppmraw	Portable Pixmap (raw format)
# *	tiffg3	TIFF/F (G3 fax)

# User-contributed drivers marked with * require hardware or software
# that is not available to Aladdin Enterprises.  Please contact the
# original contributors, not Aladdin Enterprises, if you have questions.
# Contact information appears in the driver entry below.
#
# Drivers marked with a + are maintained by Aladdin Enterprises with
# the assistance of users, since Aladdin Enterprises doesn't have access to
# the hardware for these either.

# If you add drivers, it would be nice if you kept each list
# in alphabetical order.

# Each platform-specific makefile contains a line of the form
#	DEVICE_DEVS=<dev1>.dev ... <devn>.dev
# where dev1 ... devn are the devices to be included in the build.
# You may edit this line to select any desired set of devices.
# dev1 will be used as the default device (unless overridden from
# the command line with -sDEVICE=xxx, of course.)  If you can't fit all the
# devices on a single line, you may add lines defining
#	DEVICE_DEVS2=<dev21>.dev ... <dev2n>.dev
#	DEVICE_DEVS3=<dev31>.dev ... <dev3n>.dev
# etc. up to DEVICE_DEVS9.
# Don't use continuation lines, since this may break the MS-DOS command
# processor.

# ---------------------------- End of catalog ---------------------------- #

# If you want to add a new device driver, the examples below should be
# enough of a guide to the correct form for the makefile rules.

# All device drivers depend on the following:
GDEV=$(AK) echogs$(XE) $(gserrors_h) $(gx_h) $(gxdevice_h)

# Define the header files for device drivers.  Every header file used by
# more than one device driver must be listed here.
gdevpccm_h=gdevpccm.h
gdevpcfb_h=gdevpcfb.h $(dos__h)
gdevpcl_h=gdevpcl.h
gdevsvga_h=gdevsvga.h
gdevx_h=gdevx.h

###### ------------------- MS-DOS display devices ------------------- ######

# There are really only three drivers: an EGA/VGA driver (4 bit-planes,
# plane-addressed), a SuperVGA driver (8 bit-planes, byte addressed),
# and a special driver for the S3 chip.
# To make A4 paper the default, change the compilation line from
#	$(CCD) ...
# to
#	$(CCD) -DA4

### ----------------------- EGA and VGA displays ----------------------- ###

gdevegaa.$(OBJ): gdevegaa.asm

ETEST=ega.$(OBJ) $(ega_) gdevpcfb.$(OBJ) gdevegaa.$(OBJ)
ega.exe: $(ETEST) libc$(MM).tr
	$(COMPDIR)\tlink $(LCT) $(LO) $(LIBDIR)\c0$(MM) @ega.tr @libc$(MM).tr

ega.$(OBJ): ega.c $(GDEV)
	$(CCC) -v ega.c

# The shared MS-DOS makefile defines PCFBASM as either gdevegaa.$(OBJ)
# or an empty string.

# NOTE: for direct frame buffer addressing under SCO Unix or Xenix,
# change gdevevga to gdevsco in the following line.
EGAVGA=gdevevga.$(OBJ) gdevpcfb.$(OBJ) $(PCFBASM)

gdevevga.$(OBJ): gdevevga.c $(GDEV) $(gdevpcfb_h)
	$(CCD) gdevevga.c

gdevsco.$(OBJ): gdevsco.c $(GDEV) $(gdevpcfb_h)
	$(CCD) gdevsco.c

# Common code for MS-DOS and SCO.
gdevpcfb.$(OBJ): gdevpcfb.c $(GDEV) $(MAKEFILE) $(gdevpcfb_h)
	$(CCD) -DUSE_ASM=0$(USE_ASM) gdevpcfb.c

# The EGA/VGA family includes: EGA, VGA, and
# the ATI Wonder, Tseng ET3000/4000, and Trident SuperVGA in 16-color mode.

ega.dev: $(EGAVGA)
	$(SHP)gssetdev ega $(EGAVGA)

vga.dev: $(EGAVGA)
	$(SHP)gssetdev vga $(EGAVGA)

atiw16.dev: $(EGAVGA)
	$(SHP)gssetdev atiw16 $(EGAVGA)

tseng16.dev: $(EGAVGA)
	$(SHP)gssetdev tseng16 $(EGAVGA)

tvga16.dev: $(EGAVGA)
	$(SHP)gssetdev tvga16 $(EGAVGA)

### ------------------------- SuperVGA displays ------------------------ ###

SVGA=gdevsvga.$(OBJ) $(PCFBASM)

gdevsvga.$(OBJ): gdevsvga.c $(GDEV) $(MAKEFILE) \
  $(gdevpcfb_h) $(gdevsvga_h)
	$(CCD) -DUSE_ASM=0$(USE_ASM) gdevsvga.c

# The SuperVGA family includes: ATI Wonder, S3, Trident, Tseng ET3000/4000,
# and VESA.

atiw.dev: $(SVGA)
	$(SHP)gssetdev atiw $(SVGA)

tseng.dev: $(SVGA)
	$(SHP)gssetdev tseng $(SVGA)

tvga.dev: $(SVGA)
	$(SHP)gssetdev tvga $(SVGA)

vesa.dev: $(SVGA)
	$(SHP)gssetdev vesa $(SVGA)

# The S3 driver doesn't share much code with the others.

s3vga_=$(SVGA) gdevs3ga.$(OBJ)
s3vga.dev: $(s3vga_)
	$(SHP)gssetdev s3vga $(s3vga_)

gdevs3ga.$(OBJ): gdevs3ga.c $(GDEV) $(MAKEFILE) $(gdevpcfb_h) $(gdevsvga_h)
	$(CCD) gdevs3ga.c

### ------------ The BGI (Borland Graphics Interface) device ----------- ###

cgaf.$(OBJ): $(BGIDIR)\cga.bgi
	$(BGIDIR)\bgiobj /F $(BGIDIR)\cga

egavgaf.$(OBJ): $(BGIDIR)\egavga.bgi
	$(BGIDIR)\bgiobj /F $(BGIDIR)\egavga

# Include egavgaf.$(OBJ) for debugging only.
bgi_=gdevbgi.$(OBJ) cgaf.$(OBJ)
bgi.dev: $(bgi_)
	$(SHP)gssetdev bgi $(bgi_)
	$(SHP)gsaddmod bgi -lib $(LIBDIR)\graphics

gdevbgi.$(OBJ): gdevbgi.c $(GDEV) $(MAKEFILE) $(gxxfont_h)
	$(CCC) -DBGI_LIB=$(QQ)$(BGIDIRSTR)$(QQ) gdevbgi.c

### ------------------- The Hercules Graphics display ------------------- ###

herc_=gdevherc.$(OBJ)
herc.dev: $(herc_)
	$(SHP)gssetdev herc $(herc_)

gdevherc.$(OBJ): gdevherc.c $(GDEV)
	$(CCC) gdevherc.c

###### ------------------- The Private Eye display ------------------- ######
### Note: this driver was contributed by a user:                          ###
###   please contact narf@media-lab.media.mit.edu if you have questions.  ###

pe_=gdevpe.$(OBJ)
pe.dev: $(pe_)
	$(SHP)gssetdev pe $(pe_)

gdevpe.$(OBJ): gdevpe.c $(GDEV)

###### ----------------- The MS-Windows 3.n display ------------------ ######

gdevmswn_h=gdevmswn.h $(GDEV) gp_mswin.h

# Choose one of gdevwddb or gdevwdib here.
mswin_=gdevmswn.$(OBJ) gdevmsxf.$(OBJ) gdevwdib.$(OBJ) \
  gdevemap.$(OBJ) gdevpccm.$(OBJ)
mswin.dev: $(mswin_)
	$(SHP)gssetdev mswin $(mswin_)

gdevmswn.$(OBJ): gdevmswn.c $(gdevmswn_h) $(gp_h) $(gpcheck_h) \
  $(gsprops_h) $(gdevpccm_h)

gdevmsxf.$(OBJ): gdevmsxf.c $(ctype__h) $(math__h) $(memory__h) \
  $(gdevmswn_h) $(gsutil_h) $(gxxfont_h)

# An implementation using a device-dependent bitmap.
gdevwddb.$(OBJ): gdevwddb.c $(gdevmswn_h)

# An implementation using a DIB filled by an image device.
gdevwdib.$(OBJ): gdevwdib.c $(dos__h) $(gdevmswn_h)

###### ----------------- The MS-Windows 3.n printer ------------------ ######

mswinprn_=gdevwprn.$(OBJ) gdevmsxf.$(OBJ)
mswinprn.dev: $(mswinprn_)
	$(SHP)gssetdev mswinprn $(mswinprn_)

gdevwprn.$(OBJ): gdevwprn.c $(gdevmswn_h) $(gp_h) $(gpcheck_h) \
  $(gsprops_h) $(gdevpccm_h)

###### ----------- The AT&T 3b1 Unixpc monochrome display ------------ ######
### Note: this driver was contributed by a user: please contact           ###
###       Andy Fyfe (andy@cs.caltech.edu) if you have questions.          ###

att3b1_=gdev3b1.$(OBJ)
att3b1.dev: $(att3b1_)
	$(SHP)gssetdev att3b1 $(att3b1_)

gdev3b1.$(OBJ): gdev3b1.c

###### --------------- Memory-buffered printer devices --------------- ######

# The dependency list for printers includes devs.mak because
# you can specify -DA4 to make A4 paper the default.
# See below under, e.g., gdevdjet.c.
PDEVH=$(GDEV) $(gdevprn_h) devs.mak

gdevprn.$(OBJ): gdevprn.c $(PDEVH) $(gp_h) $(gsprops_h)

### ------------------- The Apple DMP printer device ------------------- ###

appledmp_=gdevadmp.$(OBJ) gdevprn.$(OBJ)

appledmp.dev: $(appledmp_)
	$(SHP)gssetdev appledmp $(appledmp_)

### ------------ The Canon BubbleJet BJ10e and BJ200 devices ------------ ###

bj10e_=gdevbj10.$(OBJ) gdevprn.$(OBJ)

bj10e.dev: $(bj10e_)
	$(SHP)gssetdev bj10e $(bj10e_)

bj200.dev: $(bj10e_)
	$(SHP)gssetdev bj200 $(bj10e_)

gdevbj10.$(OBJ): gdevbj10.c $(PDEVH)

### -------------------------- The DigiFAX device ----------------------- ###
###    This driver outputs images in a format suitable for use with       ###
###    DigiBoard, Inc.'s DigiFAX software.  Use -sDEVICE=dfaxhigh for     ###
###    high resolution output, -sDEVICE=dfaxlow for normal output.        ###
### Note: this driver was contributed by a user: please contact           ###
###       Rick Richardson (rick@digibd.com) if you have questions.        ###

digifax_=gdevdfax.$(OBJ) gdevprn.$(OBJ)
dfaxhigh.dev: $(digifax_)
	$(SHP)gssetdev dfaxhigh $(digifax_)

dfaxlow.dev: $(digifax_)
	$(SHP)gssetdev dfaxlow $(digifax_)

gdevdfax.$(OBJ): gdevdfax.c $(GDEV) $(gdevprn_h) gdevdfg3.h

### ----------- The H-P DeskJet and LaserJet printer devices ----------- ###

### These are essentially the same device.
### You can make A4 paper the default: see below.
### NOTE: printing at full resolution (300 DPI) requires a printer
###   with at least 1.5 Mb of memory.  150 DPI only requires .5 Mb.

HPPCL=gdevprn.$(OBJ) gdevpcl.$(OBJ)
HPMONO=gdevdjet.$(OBJ) $(HPPCL)

gdevpcl.$(OBJ): gdevpcl.c $(PDEVH) $(gdevpcl_h)

# To make A4 paper the default, change the second line below this to
#	$(CCC) -DA4 gdevdjet.c
gdevdjet.$(OBJ): gdevdjet.c $(PDEVH) $(gdevpcl_h)
	$(CCC) gdevdjet.c

deskjet.dev: $(HPMONO)
	$(SHP)gssetdev deskjet $(HPMONO)

djet500.dev: $(HPMONO)
	$(SHP)gssetdev djet500 $(HPMONO)

laserjet.dev: $(HPMONO)
	$(SHP)gssetdev laserjet $(HPMONO)

ljetplus.dev: $(HPMONO)
	$(SHP)gssetdev ljetplus $(HPMONO)

### Selecting ljet2p provides TIFF (mode 2) compression on LaserJet III,
### IIIp, IIId, IIIsi, IId, and IIp. 

ljet2p.dev: $(HPMONO)
	$(SHP)gssetdev ljet2p $(HPMONO)

### Selecting ljet3 provides Delta Row (mode 3) compression on LaserJet III,
### IIIp, IIId, IIIsi.

ljet3.dev: $(HPMONO)
	$(SHP)gssetdev ljet3 $(HPMONO)

### Selecting ljet4 also provides Delta Row compression on LaserJet IV series.

ljet4.dev: $(HPMONO)
	$(SHP)gssetdev ljet4 $(HPMONO)

###- The H-P DeskJet 500C/550C and PaintJet family color printer devices -###
### Note: there are two different 500C drivers, both contributed by users.###
###   If you have questions about the djet500c driver,                    ###
###       please contact AKayser@et.tudelft.nl.                           ###
###   If you have questions about the cdj* drivers,                       ###
###       please contact g.cameron@biomed.abdn.ac.uk.                     ###

cdeskjet_=gdevcdj.$(OBJ) $(HPPCL)

cdeskjet.dev: $(cdeskjet_)
	$(SHP)gssetdev cdeskjet $(cdeskjet_)

cdjcolor.dev: $(cdeskjet_)
	$(SHP)gssetdev cdjcolor $(cdeskjet_)

cdjmono.dev: $(cdeskjet_)
	$(SHP)gssetdev cdjmono $(cdeskjet_)

cdj500.dev: $(cdeskjet_)
	$(SHP)gssetdev cdj500 $(cdeskjet_)

cdj550.dev: $(cdeskjet_)
	$(SHP)gssetdev cdj550 $(cdeskjet_)

declj250.dev: $(cdeskjet_)
	$(SHP)gssetdev declj250 $(cdeskjet_)

pj.dev: $(cdeskjet_)
	$(SHP)gssetdev pj $(cdeskjet_)

pjxl.dev: $(cdeskjet_)
	$(SHP)gssetdev pjxl $(cdeskjet_)

pjxl300.dev: $(cdeskjet_)
	$(SHP)gssetdev pjxl300 $(cdeskjet_)

# NB: you can also customise the build if required, using -DA4 (for A4 paper)
# and -DBitsPerPixel=<number> if you wish the default to be other than 24
# for the generic drivers (cdj500, cdj550, pjxl300, pjtest, pjxltest).
# E.g,. to make A4 paper the default, change the second line below this to
#	$(CCC) -DA4 gdevdjet.c
gdevcdj.$(OBJ): gdevcdj.c $(PDEVH) $(gdevpcl_h)
	$(CCC) gdevcdj.c

djet500c_=gdevdjtc.$(OBJ) $(HPPCL)
djet500c.dev: $(djet500c_)
	$(SHP)gssetdev djet500c $(djet500c_)

gdevdjtc.$(OBJ): gdevdjtc.c $(PDEVH) $(gdevpcl_h)

### ----------------- The generic Epson printer device ----------------- ###

epson_=gdevepsn.$(OBJ) gdevprn.$(OBJ)

epson.dev: $(epson_)
	$(SHP)gssetdev epson $(epson_)

eps9high.dev: $(epson_)
	$(SHP)gssetdev eps9high $(epson_)

gdevepsn.$(OBJ): gdevepsn.c $(PDEVH)

### ----------------- The IBM Proprinter printer device ---------------- ###

ibmpro.dev: $(epson_)
	$(SHP)gssetdev ibmpro $(epson_)

### -------------- The Epson LQ-2550 color printer device -------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Dave St. Clair (dave@exlog.com) if you have questions.         ###

epsonc_=gdevepsc.$(OBJ) gdevprn.$(OBJ)
epsonc.dev: $(epsonc_)
	$(SHP)gssetdev epsonc $(epsonc_)

gdevepsc.$(OBJ): gdevepsc.c $(PDEVH)

### -------------- The Epson ESC/P 2 language printer device ----------- ###
### Note: this driver was contributed by a user: if you have questions,  ###
###       please contact Richard Brown (rab@tauon.ph.unimelb.edu.au).    ###

escp2_=gdevescp.$(OBJ) gdevprn.$(OBJ)
escp2.dev: $(escp2_)
	$(SHP)gssetdev escp2 $(escp2_)

gdevescp.$(OBJ): gdevescp.c $(PDEVH)

### ------------ The H-P PaintJet color printer device ----------------- ###
### Note: this driver also supports the DEC LJ250 color printer, which   ###
###       has a PaintJet-compatible mode, and the PaintJet XL.           ###
### If you have questions about the XL, please contact Rob Reiss         ###
###       (rob@moray.berkeley.edu).                                      ###

PJET=gdevpjet.$(OBJ) $(HPPCL)

gdevpjet.$(OBJ): gdevpjet.c $(PDEVH) $(gdevpcl_h)

lj250.dev: $(PJET)
	$(SHP)gssetdev lj250 $(PJET)

paintjet.dev: $(PJET)
	$(SHP)gssetdev paintjet $(PJET)

pjetxl.dev: $(PJET)
	$(SHP)gssetdev pjetxl $(PJET)

### ------- The IBM 3852 JetPrinter color inkjet printer device -------- ###
### Note: this driver was contributed by users: please contact           ###
###       Kevin Gift (kgift@draper.com) if you have questions.           ###
### Note that the paper size that can be addressed by the graphics mode  ###
###   used in this driver is fixed at 7-1/2 inches wide (the printable   ###
###   width of the jetprinter itself.)                                   ###

jetp3852_=gdev3852.$(OBJ) gdevprn.$(OBJ)
jetp3852.dev: $(jetp3852_)
	$(SHP)gssetdev jetp3852 $(jetp3852_)

gdevjetp.$(OBJ): gdevjetp.c $(PDEVH) $(gdevpcl_h)

### ----------------- The Canon LBP-8II printer device ----------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Tom Quinn (trq@prg.oxford.ac.uk) if you have questions.        ###
### Note that the standard paper size for this driver is the European    ###
###   A4 size, not the American 8.5" x 11" size.                         ###

lbp8_=gdevlbp8.$(OBJ) gdevprn.$(OBJ)
lbp8.dev: $(lbp8_)
	$(SHP)gssetdev lbp8 $(lbp8_)

gdevlbp8.$(OBJ): gdevlbp8.c $(PDEVH)

### -------------- The DEC LN03/LA50/LA75 printer devices -------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Ulrich Mueller (ulm@vsnhd1.cern.ch) if you have questions.     ###
### For questions about LA50 and LA75: please contact                    ###
###       Ian MacPhedran (macphed@dvinci.USask.CA).                     ###
### For the LN03, you can make A4 paper the default: see below.          ###

ln03_=gdevln03.$(OBJ) gdevprn.$(OBJ)
ln03.dev: $(ln03_)
	$(SHP)gssetdev ln03 $(ln03_)

la50.dev: $(ln03_)
	$(SHP)gssetdev la50 $(ln03_)

la75.dev: $(ln03_)
	$(SHP)gssetdev la75 $(ln03_)

# To make A4 paper the default, change the second line below this to
#	$(CCC) -DA4 gdevln03.c
gdevln03.$(OBJ): gdevln03.c $(PDEVH)
	$(CCC) gdevln03.c

### -------------- The C.Itoh M8510 printer device --------------------- ###
### Note: this driver was contributed by a user: please contact Bob      ###
###       Smith <bob@snuffy.penfield.ny.us> if you have questions.       ###

m8510_=gdev8510.$(OBJ) gdevprn.$(OBJ)
m8510.dev: $(m8510_)
	$(SHP)gssetdev m8510 $(m8510_)

gdev8510.$(OBJ): gdev8510.c $(PDEVH)

### --------------------- The NEC P6 family devices -------------------- ###

necp6_=gdevnp6.$(OBJ) gdevprn.$(OBJ)
necp6.dev: $(necp6_)
	$(SHP)gssetdev necp6 $(necp6_)

gdevnp6.$(OBJ): gdevnp6.c $(PDEVH)

### ----------------- The Okidata MicroLine 182 device ----------------- ###
### Note: this driver was contributed by a user: please contact          ###
###       Maarten Koning (smeg@bnr.ca) if you have questions.            ###

oki182_=gdevo182.$(OBJ) gdevprn.$(OBJ)
oki182.dev: $(oki182_)
	$(SHP)gssetdev oki182 $(oki182_)

gdevo182.$(OBJ): gdevo182.c $(PDEVH)

### ------------- The Ricoh 4081 laser printer device ------------------ ###
### Note: this driver was contributed by users:                          ###
###       please contact kdw@oasis.icl.co.uk if you have questions.      ###

r4081_=gdev4081.$(OBJ) gdevprn.$(OBJ)
r4081.dev: $(r4081_)
	$(SHP)gssetdev r4081 $(r4081_)

gdev4081.$(OBJ): gdev4081.c $(PDEVH)

###### ------------------------ Sony devices ------------------------ ######
### Note: these drivers were contributed by users: please contact        ###
###       Mike Smolenski (mike@intertech.com) if you have questions.     ###

### ------------------- Sony NeWS frame buffer device ------------------ ###

sonyfb_=gdevsnfb.$(OBJ) gdevprn.$(OBJ)
sonyfb.dev: $(sonyfb_)
	$(SHP)gssetdev sonyfb $(sonyfb_)

gdevsnfb.$(OBJ): gdevsnfb.c $(PDEVH)

### -------------------- Sony NWP533 printer device -------------------- ###
### Note: this driver was contributed by a user: please contact Tero     ###
###       Kivinen (kivinen@joker.cs.hut.fi) if you have questions.       ###

nwp533_=gdevn533.$(OBJ) gdevprn.$(OBJ)
nwp533.dev: $(nwp533_)
	$(SHP)gssetdev nwp533 $(nwp533_)

gdevn533.$(OBJ): gdevn533.c $(PDEVH)

### ------------------------- The SPARCprinter ------------------------- ###
### Note: this driver was contributed by users: please contact Martin    ###
###       Schulte (schulte@thp.uni-koeln.de) if you have questions.      ###
###       He would also like to hear from anyone using the driver.       ###
### Please consult the source code for additional documentation.         ###

sparc_=gdevsppr.$(OBJ) gdevprn.$(OBJ)
sparc.dev: $(sparc_)
	$(SHP)gssetdev sparc $(sparc_)

gdevsppr.$(OBJ): gdevsppr.c $(PDEVH)

###### --------------------- The SunView device --------------------- ######
### Note: this driver is maintained by a user: if you have questions,    ###
###       please contact Andreas Stolcke (stolcke@icsi.berkeley.edu).    ###

sunview_=gdevsun.$(OBJ)
sunview.dev: $(sunview_)
	$(SHP)gssetdev sunview $(sunview_)
	$(SHP)gsaddmod sunview -lib suntool sunwindow pixrect

gdevsun.$(OBJ): gdevsun.c $(GDEV) $(arch_h)

### ----------------- Tektronix 4396d color printer -------------------- ###
### Note: this driver was contributed by a user: please contact          ###
###       Karl Hakimian (hakimian@haney.eecs.wsu.edu)                    ###
###       if you have questions.                                         ###

t4693d_=gdev4693.$(OBJ) gdevprn.$(OBJ)
t4693d2.dev: $(t4693d_)
	$(SHP)gssetdev t4693d2 $(t4693d_)

t4693d4.dev: $(t4693d_)
	$(SHP)gssetdev t4693d4 $(t4693d_)

t4693d8.dev: $(t4693d_)
	$(SHP)gssetdev t4693d8 $(t4693d_)

gdev4693.$(OBJ): gdev4693.c $(GDEV)

### -------------------- Tektronix ink-jet printers -------------------- ###
### Note: this driver was contributed by a user: please contact          ###
###       Karsten Spang (spang@nbivax.nbi.dk) if you have questions.     ###

tek4696_=gdevtknk.$(OBJ) gdevprn.$(OBJ)
tek4696.dev: $(tek4696_)
	$(SHP)gssetdev tek4696 $(tek4696_)

gdevtknk.$(OBJ): gdevtknk.c $(PDEVH)

### ----------------- The TruFax facsimile device ---------------------- ###
### Note: this driver was contributed by users: please contact           ###
###       Neil Ostroff (nao@maestro.bellcore.com) if you have questions. ###
### Note that the driver requires a file encode_l.o supplied by the      ###
###   makers of the TruFax product.                                      ###

trufax_=gdevtrfx.$(OBJ) gdevprn.$(OBJ) encode_l.$(OBJ)
trufax.dev: $(trufax_)
	$(SHP)gssetdev trufax $(trufax_)

gdevtrfx.$(OBJ): gdevtrfx.c $(GDEV)

###### ----------------------- The X11 device ----------------------- ######

# Aladdin Enterprises does not support Ghostview.  For more information
# about Ghostview, please contact Tim Theisen (ghostview@cs.wisc.edu).

x11_=gdevx.$(OBJ) gdevxini.$(OBJ) gdevxxf.$(OBJ) gdevemap.$(OBJ)
x11.dev: $(x11_)
	$(SHP)gssetdev x11 $(x11_)
	$(SHP)gsaddmod x11 -lib Xt X11 Xext

# See the main makefile for the definition of XINCLUDE.
GDEVX=$(GDEV) x_.h gdevx.h $(MAKEFILE)
gdevx.$(OBJ): gdevx.c $(GDEVX) $(gsprops_h) $(gsutil_h)
	$(CCC) $(XINCLUDE) gdevx.c

gdevxini.$(OBJ): gdevxini.c $(GDEVX) $(ctype__h)
	$(CCC) $(XINCLUDE) gdevxini.c

gdevxxf.$(OBJ): gdevxxf.c $(GDEVX) $(gsutil_h) $(gxxfont_h)
	$(CCC) $(XINCLUDE) gdevxxf.c

### ---------------------- The bit bucket device ----------------------- ###

bit_=gdevbit.$(OBJ) gdevprn.$(OBJ)
bit.dev: $(bit_)
	$(SHP)gssetdev bit $(bit_)

gdevbit.$(OBJ): gdevbit.c $(PDEVH)

###### ----------------------- PC file formats ---------------------- ######

gdevpccm.$(OBJ): gdevpccm.c $(AK) \
  $(gs_h) $(gsmatrix_h) $(gxdevice_h) $(gdevpccm_h)

### ------------------------- .BMP file formats ------------------------- ###

bmp_=gdevbmp.$(OBJ) gdevpccm.$(OBJ) gdevprn.$(OBJ)

gdevbmp.$(OBJ): gdevbmp.c $(PDEVH) $(gdevpccm_h)

bmpmono.dev: $(bmp_)
	$(SHP)gssetdev bmpmono $(bmp_)

bmp16.dev: $(bmp_)
	$(SHP)gssetdev bmp16 $(bmp_)

bmp256.dev: $(bmp_)
	$(SHP)gssetdev bmp256 $(bmp_)

bmp16m.dev: $(bmp_)
	$(SHP)gssetdev bmp16m $(bmp_)

### ------------------------- GIF file formats ------------------------- ###

GIF=gdevgif.$(OBJ) gdevpccm.$(OBJ) gdevprn.$(OBJ)

gdevgif.$(OBJ): gdevgif.c $(PDEVH) $(gdevpccm_h)

gifmono.dev: $(GIF)
	$(SHP)gssetdev gifmono $(GIF)

gif8.dev: $(GIF)
	$(SHP)gssetdev gif8 $(GIF)

### ------------------------- PCX file formats ------------------------- ###

pcx_=gdevpcx.$(OBJ) gdevpccm.$(OBJ) gdevprn.$(OBJ)

gdevpcx.$(OBJ): gdevpcx.c $(PDEVH) $(gdevpccm_h) $(gxlum_h)

pcxmono.dev: $(pcx_)
	$(SHP)gssetdev pcxmono $(pcx_)

pcxgray.dev: $(pcx_)
	$(SHP)gssetdev pcxgray $(pcx_)

pcx16.dev: $(pcx_)
	$(SHP)gssetdev pcx16 $(pcx_)

pcx256.dev: $(pcx_)
	$(SHP)gssetdev pcx256 $(pcx_)

###### ------------------- Portable Bitmap devices ------------------ ######
### For more information, see the pbm(5), pgm(5), and ppm(5) man pages.  ###

pxm_=gdevpbm.$(OBJ) gdevprn.$(OBJ)

gdevpbm.$(OBJ): gdevpbm.c $(PDEVH) $(gxlum_h)

### Portable Bitmap (PBM, plain or raw format, magic numbers "P1" or "P4")

pbm.dev: $(pxm_)
	$(SHP)gssetdev pbm $(pxm_)

pbmraw.dev: $(pxm_)
	$(SHP)gssetdev pbmraw $(pxm_)

### Portable Graymap (PGM, plain or raw format, magic numbers "P2" or "P5")

pgm.dev: $(pxm_)
	$(SHP)gssetdev pgm $(pxm_)

pgmraw.dev: $(pxm_)
	$(SHP)gssetdev pgmraw $(pxm_)

### Portable Pixmap (PPM, plain or raw format, magic numbers "P3" or "P6")

ppm.dev: $(pxm_)
	$(SHP)gssetdev ppm $(pxm_)

ppmraw.dev: $(pxm_)
	$(SHP)gssetdev ppmraw $(pxm_)

### -------------------------- TIFF/F device ---------------------------- ###
###    This driver outputs images in a TIFF format 			  ###
###    Use -sDEVICE=tiffg3 and						  ###
###	  -r204x98 for low resolution output, or			  ###
###	  -r204x196 for high resolution output				  ###
###    Note also that 3 page sizes are understood: letter, A4, and B4	  ###
### Note: this driver was contributed by a user: please contact           ###
###       Sam Leffler (sam@sgi.com) if you have questions.        	  ###

tiffg3_=gdevtiff.$(OBJ) gdevprn.$(OBJ)
tiffg3.dev: $(tiffg3_)
	$(SHP)gssetdev tiffg3 $(tiffg3_)
gdevtiff.$(OBJ): gdevtiff.c $(GDEV) $(gdevprn_h) gdevdfg3.h gdevtiff.h
#    Copyright (C) 1990, 1992, 1993 Aladdin Enterprises.  All rights reserved.
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

# Partial makefile for Ghostscript, common to all Unix configurations.

# This is the last part of the makefile for Unix configurations.
# Since Unix make doesn't have an 'include' facility, we concatenate
# the various parts of the makefile together by brute force (in tar_cat).

# The following prevents GNU make from constructing argument lists that
# include all environment variables, which can easily be longer than
# brain-damaged system V allows.

.NOEXPORT:

# -------------------------------- Library -------------------------------- #

## The Unix platforms

# We have to include a test for the existence of sys/time.h,
# because some System V platforms don't have it.

# All reasonable Unix platforms.
unix__=gp_nofb.$(OBJ) gp_unix.$(OBJ) gdevpipe.$(OBJ)
unix_.dev: $(unix__)
	$(SHP)gssetmod unix_ $(unix__)
	$(SHP)gsaddmod unix_ -fdev pipe

gp_unix.$(OBJ): gp_unix.c $(AK) $(memory__h) $(string__h) $(gx_h) $(gp_h) \
 $(gsutil_h) $(stat__h) $(time__h)
	if ( test -f /usr/include/sys/time.h ) then $(CCC) gp_unix.c;\
	else $(CCC) -DNOSYSTIME gp_unix.c; fi

gdevpipe.$(OBJ): gdevpipe.c $(AK) $(stdio__h) $(gstypes_h) \
  $(filedev_h) $(stream_h)

# Brain-damaged System V platforms.
sysv__=gp_nofb.$(OBJ) gp_unix.$(OBJ) gp_sysv.$(OBJ)
sysv_.dev: $(sysv__)
	$(SHP)gssetmod sysv_ $(sysv__)

gp_sysv.$(OBJ): gp_sysv.c $(time__h) $(AK)
	if ( test -f /usr/include/sys/time.h ) then $(CCC) gp_sysv.c;\
	else $(CCC) -DNOSYSTIME gp_sysv.c; fi

# -------------------------- Auxiliary programs --------------------------- #

ansi2knr$(XE): ansi2knr.c $(stdio__h) $(string__h) $(malloc__h)
	$(CC) -o ansi2knr$(XE) $(CFLAGS) ansi2knr.c

echogs$(XE): echogs.c
	$(CC) -o echogs$(XE) $(CFLAGS) echogs.c

# On the RS/6000 (at least), compiling genarch.c with gcc with -O
# produces a buggy executable.
genarch$(XE): genarch.c
	$(CC) -o genarch$(XE) genarch.c

genconf$(XE): genconf.c
	$(CC) -o genconf$(XE) genconf.c

# ----------------------------- Main program ------------------------------ #

BEGINFILES=
CCBEGIN=$(CCC) *.c

# Interpreter main program

GSUNIX=gs.$(OBJ) gsmain.$(OBJ) $(INT) $(LIBGS)

# The second call on echogs writes a \.  This is the only
# way to do it that works with all flavors of shell!
$(GS)$(XE): $(GSUNIX) ld.tr echogs $(ALL_DEVS)
	./echogs -n - $(CC) $(LDFLAGS) $(XLIBDIRS) -o gs $(GSUNIX) >_temp_
	./echogs -x 205c >>_temp_
	cat ld.tr >>_temp_
	./echogs - $(EXTRALIBS) -lm >>_temp_
	$(SH) <_temp_

# Installation

TAGS:
	etags -t *.c *.h

docdir=$(gsdatadir)/doc
exdir=$(gsdatadir)/examples

install: $(GS)
	-mkdir $(bindir)
	for f in $(GS) gsbj gsdj gslj gslp gsnd bdftops font2c ps2ascii ps2epsi ; do $(INSTALL_PROGRAM) $$f $(bindir)/$$f ; done
	-mkdir $(datadir)
	-mkdir $(gsdatadir)
	for f in README gslp.ps gs_init.ps gs_dps1.ps gs_fonts.ps gs_lev2.ps gs_statd.ps gs_type0.ps gs_dbt_e.ps gs_sym_e.ps quit.ps Fontmap uglyr.gsf bdftops.ps decrypt.ps font2c.ps impath.ps landscap.ps level1.ps prfont.ps ps2ascii.ps ps2epsi.ps ps2image.ps pstoppm.ps showpage.ps type1ops.ps wrfont.ps ; do $(INSTALL_DATA) $$f $(gsdatadir)/$$f ; done
	-mkdir $(docdir)
	for f in NEWS ansi2knr.1 devices.doc drivers.doc fonts.doc gs.1 hershey.doc history.doc humor.doc language.doc lib.doc make.doc ps2epsi.doc psfiles.doc readme.doc use.doc xfonts.doc ; do $(INSTALL_DATA) $$f $(docdir)/$$f ; done
	-mkdir $(exdir)
	for f in chess.ps cheq.ps colorcir.ps golfer.ps escher.ps snowflak.ps tiger.ps ; do $(INSTALL_DATA) $$f $(exdir)/$$f ; done
