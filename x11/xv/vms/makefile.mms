#	MMS Description file for xv (v3.00)
#	Written by Rick Dyson (dyson@iowasp.physics.uiowa.edu)
#	Last Modified: 30-APR-1992 for v2.21
#	                5-OCT-1992 for v2.21 (export.lcs.mit.edu version
#                                  of xv-2.21 seemed to change about
#                                  25-Sep-1992 without version number
#                                  changing.
#                       8-FEB-1993 for v2.21b
#                                  ALPHA support is in ALPHA.MMS
#                       2-MAR-1993 for v3.00
#                      15-APR-1993 for v3.00 (DEC C changes)
#
# 	Modeled after the original Unix Makefile for xv
#	Most of the Unix comments have been left intact to help debug any
#	problems.

# your C compiler (and options) of choice
# Remember:  if you change the C compiler (to gcc, or whatever), be sure to
# do the same thing to the Makefile in the 'jpeg' and 'tiff' subdirectories
#
# For DEC C users, you must add a MACRO qualifier to the command line,
# i.e.,
#       MMS /Description = Makefile.mms /Macro = DECC

CC = cc


# BE SURE TO SET THIS TO YOUR SITE'S DESTINATION DIRECTORY...!!!
BINDIR = Sys$Disk:[]

################ CONFIGURATION OPTIONS #################

# if, for whatever reason, you're unable to get the JPEG library to compile
# on your machine, *COMMENT OUT* the following lines.
#
# Also, comment out the JPEGLIB dependancy below.
#
#  VMS MMS USERS!!!
#
# 	if you don't use the JPEG package as supplied with XV, you
# 	will need fill in the complete directory specifications for
#	BOTH JPEGDIR and XVDIR!!
#
XVDIR = [XV-3_00]
JPEGDIR = [.JPEG]
JPEG = ,HAVE_JPEG,HAVE_STDC
JPEGLIB = $(JPEGDIR)LIBJPEG.OLB
JPEGINCLUDE = ,$(JPEGDIR)


# if, for whatever reason, you're unable to get the TIFF library to compile
# on your machine, *COMMENT OUT* the following lines
#
# Also, comment out the LIBTIFF dependancy at the end of this Makefile
#
#  VMS MMS USERS!!!
#
# 	if you don't use the TIFF package as supplied with XV, you
# 	will need fill in the complete directory specifications for
#	BOTH TIFFDIR and XVDIR!!
#
TIFF = ,HAVE_TIFF
TIFFDIR = [.TIFF]
TIFFLIB = $(TIFFDIR)LIBTIFF.OLB
TIFFINCLUDE = ,$(TIFFDIR)


# PostScript file input support:
#
# if you have the 'ghostscript' package installed (version 2.4 or later),
# you can use it let XV display postscript files.  Note: to do so, you're
# 'gs' *must* be built with 'pbmraw','pgmraw', and 'ppmraw' support, which
# is *NOT* turned on by default when you build gs.  It's easy.  See the gs
# distribution for how-to information.  (currently, it's just a matter of
# adding 'pbmraw.dev pgmraw.dev ppmraw.dev' to the DEVICE_DEVS line in the
# ghostscript Makefile, and typing make).  Also note that if your version
# of gs supports 'pnmraw' you should add that support as well, and change
# the GS_DEV line below to 'pnmraw' for optimum performance.  (Currently,
# this device isn't in ghostscript, but it should be appearing in a new
# version.
#
# if you wish to use gs, uncomment the following lines, and make sure that
# GS_PATH specifies the complete path to your gs executable.  GS_LIB should
# be set if there's some other gs libs that should be searched, but aren't
# by default.  (In which case you should probably just fix your 'gs' so it
# looks in the right places without being told...)
# Note that it is necessary to put a backslash in front of the " chars

GS_PATH=,"GS_PATH=""gs"""
#GS_LIB=\"\"
GS_DEV=,"GS_DEV=""pbmraw"""


# By default, backing_store is turned on ('WhenMapped') for several XV windows.
# This is generally a good performance improvement, however, it has been known
# to crash some servers (most notably X11R3 running on an HP), so this is left
# as something that you could turn off, if necessary.
BACKING_STORE = ,BACKING_STORE


# if, for whatever reason, you're unable to get the PDS/VICAR support
# to compile (xvpds.c, and vdcomp.c), *COMMENT OUT* the following line,
# and also remove 'vdcomp' from the 'all:' dependancy
PDS = ,HAVE_PDS


DEFS = /Define = (VMS$(JPEG)$(PDS)$(TIFF)$(BACKINGSTORE)$(GS_PATH)$(GS_DEV))
INCS = /Include = ([]$(JPEGINCLUDE)$(TIFFINCLUDE))

OPTIMIZE = /Optimize
.ifdef DECC
OPTIMIZE = /Optimize /VAXC
.endif
DEBUG = /NoDebug
CFLAGS = $(CFLAGS) $(DEFS) $(INCS) $(DEBUG) $(OPTIMIZE)
LINKFLAGS = $(LINKFLAGS) $(DEBUG)
OPTS = Sys$Disk:[]OPTIONS.OPT
XVLIB = LIBXV.OLB

BITMAPS = [.bitmaps]grasp [.bitmaps]penn [.bitmaps]down [.bitmaps]down1 \
	  [.bitmaps]up [.bitmaps]up1 [.bitmaps]scrlgray [.bitmaps]gray50 \
	  [.bitmaps]gray25 [.bitmaps]i_fifo [.bitmaps]i_chr [.bitmaps]i_dir \
	  [.bitmaps]i_blk [.bitmaps]i_lnk [.bitmaps]i_sock [.bitmaps]i_reg \
	  [.bitmaps]i_exe [.bitmaps]rb_frame [.bitmaps]rb_frame1 [.bitmaps]rb_body \
	  [.bitmaps]rb_top [.bitmaps]rb_dtop [.bitmaps]rb_bot [.bitmaps]rb_dbot \
	  [.bitmaps]rb_dot [.bitmaps]uph [.bitmaps]uph1 [.bitmaps]downh \
	  [.bitmaps]downh1 \
	  [.bitmaps]fc_left [.bitmaps]fc_leftm [.bitmaps]fc_mid [.bitmaps]fc_midm \
	  [.bitmaps]fc_right [.bitmaps]fc_rightm [.bitmaps]fc_left1 \
	  [.bitmaps]fc_left1m [.bitmaps]fc_right1 [.bitmaps]fc_right1m \
	  [.bitmaps]icon [.bitmaps]dial_cw1 [.bitmaps]dial_cw2 [.bitmaps]dial_ccw1 \
	  [.bitmaps]dial_ccw2 [.bitmaps]iconmask [.bitmaps]gf1_addh \
	  [.bitmaps]gf1_delh [.bitmaps]gf1_line [.bitmaps]gf1_rst \
	  [.bitmaps]gf1_spln [.bitmaps]gf1_gamma \
	  [.bitmaps]h_rotl [.bitmaps]h_rotr [.bitmaps]h_sinc [.bitmaps]h_sdec \
	  [.bitmaps]h_flip [.bitmaps]cb_check \
	  [.bitmaps]h_sat [.bitmaps]h_desat [.bitmaps]root_weave \
          [.bitmaps]cboard50 [.bitmaps]mb_chk [.bitmaps]fliph [.bitmaps]flipv \
	  [.bitmaps]p10 [.bitmaps]m10

BITMAPLIST = grasp,penn,down,down1,up,up1,scrlgray,gray50,gray25,i_fifo,i_chr,i_dir,i_blk,i_lnk,i_sock,i_reg,i_exe,rb_frame,rb_frame1,rb_body,rb_top,rb_dtop,rb_bot,rb_dbot,rb_dot,uph,uph1,downh,downh1,fc_left,fc_leftm,fc_mid, \
             fc_midm,fc_right,fc_rightm,fc_left1,fc_left1m,fc_right1,fc_right1m,icon,dial_cw1,dial_cw2,dial_ccw1,dial_ccw2,iconmask,gf1_addh,gf1_delh,gf1_line,gf1_rst,gf1_spln,gf1_gamma,h_rotl,h_rotr,h_sinc,h_sdec,h_flip, \
             cb_check,h_sat,h_desat,root_weave,cboard50,mb_chk,fliph,flipv,p10,m10

OBJS = 	xv.obj,xvevent.obj,xvroot.obj,xvmisc.obj,xvimage.obj,xvcolor.obj, \
        xvsmooth.obj,xv24to8.obj,xvgif.obj,xvpm.obj,xvinfo.obj,xvctrl.obj, \
        xvscrl.obj,xvalg.obj,xvgifwr.obj,xvdir.obj,xvbutt.obj,xvpbm.obj, \
        xvxbm.obj,xvgam.obj,xvbmp.obj,xvdial.obj,xvgraf.obj,xvsunras.obj, \
        xvjpeg.obj,xvps.obj,xvpopup.obj,xvdflt.obj,xvtiff.obj,xvtiffwr.obj, \
        xvpds.obj,xvrle.obj,xviris.obj,xvgrab.obj,xvbrowse.obj, \
        xvtext.obj,xvpcx.obj,vms.obj

OBJLIST = xv.obj,xvevent.obj,xvroot.obj,xvmisc.obj,xvimage.obj,xvcolor.obj,xvsmooth.obj,xv24to8.obj,xvgif.obj,xvpm.obj,xvinfo.obj,xvctrl.obj,xvscrl.obj,xvalg.obj,xvgifwr.obj,xvdir.obj,xvbutt.obj,xvpbm.obj,xvxbm.obj,xvgam.obj,xvbmp.obj,xvdial.obj,xvgraf.obj,xvsunras.obj,xvjpeg.obj,xvps.obj,xvpopup.obj,xvdflt.obj,xvtiff.obj,xvtiffwr.obj,xvpds.obj,xvrle.obj,xviris.obj,xvgrab.obj,xvbrowse.obj,xvtext.obj,xvpcx.obj,vms.obj

MISC = readme. changelog. ideas.

.first
	@- Define Sys Sys$Library
	@- Define X11 DECW$Include
	@ If F$GetSYI ("NODE_HWTYPE") .eqs. "ALPH" Then Exit

all : 		lib xv bggen decompress help
	@ Continue

lib :   	$(JPEGLIB) $(TIFFLIB) $(XVLIB)
	@ Continue

xv :		xv.exe
	@ Continue

bggen :		bggen.exe
	@ Continue

help :		xv.hlb
	@ Continue

decompress :	decompress.exe vdcomp.exe
	@ Continue

bggen.exe : 	bggen.obj $(XVLIB) $(OPTS)
	$(LINK) $(LINKFLAGS) bggen.obj,$(XVLIB)/Library,$(OPTS)/Option

xv.exe : 	xv.obj $(XVLIB) $(JPEGLIB) $(TIFFLIB) $(OPTS) 
	$(LINK) $(LINKFLAGS) xv.obj,$(XVLIB)/Library,$(JPEGLIB)/Library,$(TIFFLIB)/Library,$(OPTS)/Option

$(JPEGLIB) :
	Set Default $(JPEGDIR)
	$(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS LIBJPEG.OLB
	Set Default $(XVDIR)

$(TIFFLIB) :
	Set Default $(TIFFDIR)
	$(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS LIBTIFF.OLB
	Set Default $(XVDIR)

$(XVLIB) :	bitmaps.h $(OBJS)
        If "''F$Search ("$(XVLIB)")'" .eqs. "" Then Library /Create $(XVLIB)
	Library /Replace $(XVLIB) $(OBJLIST)

decompress.exe :	decompress.obj
	$(LINK) $(LINKFLAGS) decompress.obj,$(OPTS)/Option

vdcomp.exe :	vdcomp.obj
	$(LINK) $(LINKFLAGS) vdcomp.obj,$(OPTS)/Option

#	various dependencies
#$(OBJS) :   		xv.h
xv.obj :		bitmaps.h
xvbutt.obj :  		bitmaps.h
xvctrl.obj :  		bitmaps.h
xvdial.obj :		bitmaps.h
xvgraf.obj :		bitmaps.h
xvinfo.obj :  		bitmaps.h
xvmisc.obj :  		bitmaps.h
xvpm.obj :		pm.h
xvscrl.obj :  		bitmaps.h
xv.hlb :		xv.hlp
vms.obj :		includes.h dirent.h

xvdflt.obj :            [.bitmaps]xvpic_logo_top [.bitmaps]xvpic_logo_bot [.bitmaps]xvpic_logo_out
xvdflt.obj :            [.bitmaps]xvpic_rev [.bitmaps]xvpic_jhb
xvdflt.obj :            [.bitmaps]xf_left [.bitmaps]xf_right

bitmaps.h : 	$(BITMAPS)
	Set Default [.Bitmaps]
	- Copy $(BITMAPLIST) [-]bitmaps.h
	Set Default [-]

install :	xv.exe vdcomp.exe bggen.exe decompress.exe
	Copy *.exe $(BINDIR)

clean :
	@- Set Protection = Owner:RWED *.obj,*.*;-1
	- Delete /NoConfirm /NoLog *.obj;*,*.exe;*
	- Purge /NoConfirm /NoLog
	Set Default [.JPEG]
	MMS /Description = MAKEFILE.MMS clean
	Set Default [-.TIFF]
	MMS /Description = MAKEFILE.MMS clean
	Set Default [-]
