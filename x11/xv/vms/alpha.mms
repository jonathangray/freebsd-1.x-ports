#       MMS Description file for xv (v2.45)
#       Written by Rick Dyson (dyson@iowasp.physics.uiowa.edu)
#       Last Modified: 30-APR-1992 for v2.21
#                       5-OCT-1992 for v2.21 (export.lcs.mit.edu version
#                                  of xv-2.21 seemed to change about
#                                  25-Sep-1992 without version number
#                                  changing.
#                       8-FEB-1993 for v2.21b
#                                  ALPHA support is in ALPHA.MMS
#                      19-FEB-1993 for v2.45
#                       2-APR-1993 for v3.00
#
#       Modeled after the original Unix Makefile for xv
#       Most of the Unix comments have been left intact to help debug any
#       problems.

# your C compiler (and options) of choice
# Remember:  if you change the C compiler (to gcc, or whatever), be sure to
# do the same thing to the Makefile in the 'jpeg' and 'tiff' subdirectories

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
#       if you don't use the JPEG package as supplied with XV, you
#       will need fill in the complete directory specifications for
#       BOTH JPEGDIR and XVDIR!!
#

XVDIR = Sys$Disk:[]
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
#       if you don't use the TIFF package as supplied with XV, you
#       will need fill in the complete directory specifications for
#       BOTH TIFFDIR and XVDIR!!
#
TIFF = ,HAVE_TIFF
TIFFDIR = [.TIFF]
TIFFLIB = $(TIFFDIR)LIBTIFF.OLB
TIFFINCLUDE = ,$(TIFFDIR)


# By default, backing_store is turned on ('WhenMapped') for several XV windows.
# This is generally a good performance improvement, however, it has been known
# to crash some servers (most notably X11R3 running on an HP), so this is left
# as something that you could turn off, if necessary.
BACKING_STORE = ,BACKING_STORE


# if, for whatever reason, you're unable to get the PDS/VICAR support
# to compile (xvpds.c, and vdcomp.c), *COMMENT OUT* the following line,
# and also remove 'vdcomp' from the 'all:' dependancy
PDS = ,HAVE_PDS


DEFS = /Define = (VMS$(JPEG)$(PDS)$(TIFF)$(BACKINGSTORE))
INCS = /Include = ([]$(JPEGINCLUDE)$(TIFFINCLUDE))

OPTIMIZE = /Optimize
DEBUG = /NoDebug
ALPHA_STUFF = /Warnings = NoInformationals /Standard = VAXC /NoList
CFLAGS = $(CFLAGS) $(DEFS) $(INCS) $(ALPHA_STUFF) $(DEBUG) $(OPTIMIZE)
LINKFLAGS = $(LINKFLAGS) $(DEBUG)
OPTS = Sys$Disk:[]ALPHA_OPTIONS.OPT
XVLIB = LIBXV.OLB

BITMAPS = [.bitmaps]grasp [.bitmaps]penn [.bitmaps]down [.bitmaps]down1 \
          [.bitmaps]up [.bitmaps]up1 [.bitmaps]scrlgray [.bitmaps]gray50 \
          [.bitmaps]gray25 [.bitmaps]i_fifo [.bitmaps]i_chr [.bitmaps]i_dir \
          [.bitmaps]i_blk [.bitmaps]i_lnk [.bitmaps]i_sock [.bitmaps]i_reg \
          [.bitmaps]i_exe [.bitmaps]rb_frame [.bitmaps]rb_frame1 [.bitmaps]rb_body \
          [.bitmaps]rb_top [.bitmaps]rb_dtop [.bitmaps]rb_bot [.bitmaps]rb_dbot\
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

OBJS =  xvevent.obj,xvroot.obj,xvmisc.obj,xvimage.obj,xvcolor.obj, \
        xvsmooth.obj,xv24to8.obj,xvgif.obj,xvpm.obj,xvinfo.obj,xvctrl.obj, \
        xvscrl.obj,xvalg.obj,xvgifwr.obj,xvdir.obj,xvbutt.obj,xvpbm.obj, \
        xvxbm.obj,xvgam.obj,xvbmp.obj,xvdial.obj,xvgraf.obj,xvsunras.obj, \
        xvjpeg.obj,xvps.obj,xvpopup.obj,xvdflt.obj,xvtiff.obj,xvtiffwr.obj, \
        xvpds.obj,xvrle.obj,xviris.obj,xvgrab.obj,xvbrowse.obj, \
        xvtext.obj,xvpcx.obj,vms.obj

OBJLIST = xvevent.obj,xvroot.obj,xvmisc.obj,xvimage.obj,xvcolor.obj,xvsmooth.obj,xv24to8.obj,xvgif.obj,xvpm.obj,xvinfo.obj,xvctrl.obj,xvscrl.obj,xvalg.obj,xvgifwr.obj,xvdir.obj,xvbutt.obj,xvpbm.obj,xvxbm.obj,xvgam.obj,xvbmp.obj,xvdial.obj,xvgraf.obj,xvsunras.obj,xvjpeg.obj,xvps.obj,xvpopup.obj,xvdflt.obj,xvtiff.obj,xvtiffwr.obj,xvpds.obj,xvrle.obj,xviris.obj,xvgrab.obj,xvbrowse.obj,xvtext.obj,xvpcx.obj,vms.obj

MISC = readme. changelog. ideas.

.first
        @ If F$GetSYI ("NODE_HWTYPE") .nes. "ALPH" Then Exit
        @- Define Sys Sys$Library
        @- Define X11 DECW$Include

all :           lib xv bggen decompress help
        @ Continue

lib :           $(JPEGLIB) $(TIFFLIB) $(XVLIB)
        @ Continue

xv :            xv.exe
        @ Continue

bggen :         bggen.exe
        @ Continue

help :          xv.hlb
        @ Continue

decompress :    decompress.exe vdcomp.exe
        @ Continue

bggen.exe :     bggen.obj $(XVLIB) $(OPTS)
        $(LINK) $(LINKFLAGS) bggen.obj,$(OPTS)/Option

xv.exe :        xv.obj $(XVLIB) $(JPEGLIB) $(TIFFLIB) $(OPTS)
        $(LINK) $(LINKFLAGS) xv.obj,$(OPTS)/Option

$(JPEGLIB) :
        Set Default $(JPEGDIR)
        $(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS LIBJPEG.OLB
        Set Default [-]
 
$(TIFFLIB) :
        Set Default $(TIFFDIR)
        $(MMS) $(MMSDEFAULTS) /Description = MAKEFILE.MMS LIBTIFF.OLB
        Set Default [-]

$(XVLIB) :      bitmaps.h $(OBJS)
        If "''F$Search ("$(XVLIB)")'" .eqs. "" Then Library /Create $(XVLIB)
        Library /Replace $(XVLIB) $(OBJLIST)

decompress.exe :        decompress.obj
        $(LINK) $(LINKFLAGS) decompress.obj,$(OPTS)/Option

vdcomp.exe :    vdcomp.obj
        $(LINK) $(LINKFLAGS) vdcomp.obj,$(OPTS)/Option

#       various dependencies
$(OBJS) :               xv.h
xv.obj :                bitmaps.h
xvpm.obj   :            pm.h
xvbutt.obj :            bitmaps.h
xvctrl.obj :            bitmaps.h
xvdial.obj :            bitmaps.h
xvgam.obj  :            bitmaps.h
xvpopup.obj :		bitmaps.h
xvgraf.obj :            bitmaps.h
xvinfo.obj :            bitmaps.h
xvmisc.obj :            bitmaps.h
xvroot.obj :		bitmaps.h
xvscrl.obj :            bitmaps.h
xv.hlb :                xv.hlp
vms.obj :               includes.h dirent.h

xvdflt.obj :            [.bitmaps]xvpic_logo_top [.bitmaps]xvpic_logo_bot [.bitmaps]xvpic_logo_out
xvdflt.obj :            [.bitmaps]xvpic_rev [.bitmaps]xvpic_jhb
xvdflt.obj :            [.bitmaps]xf_left [.bitmaps]xf_right

bitmaps.h :     $(BITMAPS)
        Set Default [.Bitmaps]
        - Copy $(BITMAPLIST) [-]bitmaps.h
        Set Default [-]

install :       xv.exe vdcomp.exe bggen.exe decompress.exe
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
