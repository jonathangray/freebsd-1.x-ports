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
