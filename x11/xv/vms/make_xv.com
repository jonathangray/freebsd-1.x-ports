$ If F$Mode () .eqs. "INTERACTIVE"
$   Then
$       VERIFY = F$Verify (0)
$   Else
$       VERIFY = F$Verify (1)
$ EndIf
$ On Control_Y Then GoTo EXIT
$ On Error     Then GoTo EXIT
$!========================================================================
$!
$!  Name      : MAKE_XV.COM
$!
$!  Purpose   : Compile and Link XV (v3.00) under VMS
$!  Suggested usage: @ MAKE_XV.COM
$!                OR
$!                   Submit /NoPrint /Log = Sys$Disk:[] /Notify MAKE_XV.COM
$!
$!  Created:  9-JAN-1992   by David Jones (jonesd@kcgl1.eng.ohio-state.edu)
$!  Updated: 19-JAN-1992   by Rick Dyson  (dyson@iowasp.physics.uiowa.edu)
$!  Updated:  9-MAR-1992   by Rick Dyson  for xv v2.11
$!  Updated: 28-APR-1992   by Rick Dyson  for xv v2.20a
$!  Updated: 30-APR-1992   by Rick Dyson  for xv v2.21
$!  Updated: 12-FEB-1993   by Rick Dyson  for xv v2.21b and VMS ALPHA support
$!                                        ALPHA support from Clark B. Merrill
$!                                                   (merrill@stsci.edu)
$!  Updated: 24-FEB-1993   by Rick Dyson  for xv v2.45
$!  Updated: 24-MAR-1993   by Rick Dyson  for xv v3.00
$!
$!========================================================================
$ THIS_PATH = F$Element (0, "]", F$Environment ("PROCEDURE")) + "]"
$ Set Default 'THIS_PATH'
$ If F$Trnlnm ("X11") .eqs. "" Then Define X11 DECW$Include
$! Test for ALPHA or VAX
$ If F$GetSyi ("HW_MODEL") .gt. 1023 
$   Then        ! it's an ALPHA
$       Define /NoLog Sys DECC$Library_Include
$       ALPHA_STUFF = "/Warnings = NoInformationals /Optimize = (Level = 4) /Standard = VAXC"
$       ALPHA_OPT = "ALPHA_"
$   Else        ! it's a VAX
$               ! check for DEC C  if DEC C use the VAX C option
$       If F$Trnlnm ("DECC$Library_Include") .nes. ""
$           Then
$               ALPHA_STUFF = "/VAXC /Optimize"
$           Else
$               ALPHA_STUFF = "/Optimize"
$       Endif
$       Define /NoLog Sys Sys$Share
$       ALPHA_OPT = ""
$   EndIf
$!  If there is an access violation when starting up XV on an ALPHA change
$!  optimization level to /Optimize = (Level = 1).  This problem occurred 
$!  at XV 2.45 and on DEC C version 1.
$!  It is posible to compile everything except XV.C at /Opt = (level=4),
$!  and then recompile XV.C at 1.  This will work also
$!                                                      cbm  3/25/93
$!
$!USER CUSTOMIZING POINT!!!!!
$!
$!
$ CC := CC 'ALPHA_STUFF' /NoList /NoDebug /Define = (VMS,HAVE_JPEG,HAVE_STDC,INCLUDES_ARE_ANSI,HAVE_PDS,HAVE_TIFF,BACKING_STORE) /Include = ([],[.JPEG],[.TIFF])
$!
$ sources = "xvevent,xvroot,xvmisc,xvimage,xvcolor,xvsmooth,xv24to8,"     + -
            "xvgif,xvpm,xvinfo,xvctrl,xvscrl,xvalg,xvgifwr,xvdir,xvbutt," + -
            "xvpbm,xvxbm,xvgam,xvbmp,xvdial,xvgraf,xvsunras,xvjpeg,xvps," + -
            "xvpopup,xvdflt,xvtiff,xvtiffwr,xvpds,xvrle,xviris,xvgrab,"   + -
            "xvbrowse,xvtext,xvpcx,vms"
$!
$ If F$Search ("bitmaps.h") .eqs. "" Then GoSub COPY_BITMAPS
$ new_objects = ""
$ If F$Search ("[.jpeg]libjpeg.olb") .eqs. ""
$   Then
$       Set Default [.jpeg]
$       Write Sys$Output "Building JPEG library..."
$       @ [-]MAKE_JPEG.COM
$       Set Default [-]
$       new_objects = ",[.JPEG]LIBJPEG.OLB/Library"
$ EndIf
$ If F$Search ("[.tiff]libtiff.olb") .eqs. ""
$   Then
$       Set Default [.tiff]
$       Write Sys$Output "Building TIFF library..."
$       @ [-]MAKE_TIFF.COM
$       Set Default [-]
$       new_objects = ",[.TIFF]LIBTIFF.OLB/Library"
$ EndIf
$!
$   Write Sys$Output "Making object library ..."
$      If F$Search ("LIBXV.OLB") .eqs. "" Then Library /Create LIBXV.OLB
$!
$!  search for missing object files.
$!
$ sndx = 0
$NEXT_SOURCE:
$   sfile = F$Element (sndx, ",", sources)
$   sndx = sndx + 1
$   If sfile .eqs. "," Then GoTo SOURCES_DONE
$   ofile = F$Parse (".OBJ", sfile)
$   If F$Search (ofile) .nes. "" Then GoTo NEXT_SOURCE
$   Write Sys$Output "Compiling ", sfile, ".c ..."
$   CC 'sfile'.c
$   If F$Search (ofile) .nes. "" Then new_objects = new_objects + "," + sfile
$   Library /Replace LIBXV.OLB 'sfile'.obj
$   GoTo NEXT_SOURCE
$!
$SOURCES_DONE:
$   If new_objects .eqs. "" .and. p1 .eqs. "" Then GoTo EXIT
$   new_objects = new_objects - ","
$!
$   Write Sys$Output "Building decompress.c ..."
$   CC decompress.c
$   Link /NoMap /Executable = decompress.exe decompress,'ALPHA_OPT'options/Option
$   Write Sys$Output "Building vdcomp.c ..."
$   CC vdcomp.c
$   Link /NoMap /Executable = vdcomp.exe vdcomp,'ALPHA_OPT'options/Option
$   Write Sys$Output "Building bggen.c ..."
$   CC bggen.c
$   Link /NoMap /Executable = bggen.exe bggen,'ALPHA_OPT'options/Option
$   Write Sys$Output "Building xv.c ..."
$   CC xv.c
$   Write Sys$Output "Linking new XV image..."
$   If F$GetSyi ("HW_MODEL") .gt. 1023 
$       Then        ! it's an ALPHA
$           Link /NoMap /Executable = xv.exe xv,'ALPHA_OPT'options/Option 'p1'
$       Else        ! it's a VAX
$           Link /NoMap /Executable = xv.exe xv,'ALPHA_OPT'options/Option 'p1'
$   EndIf
$   If "''F$Search ("xv.hlb")'" .eqs. "" Then Library /Create /Help xv.hlb
$   Library /Replace /Help xv.hlb xv.hlp
$   GoTo Exit
$!
$! subroutine to generate new bitmaps.h file.
$!
$COPY_BITMAPS:
$   Create bitmaps.h
$   bmlist = "grasp,penn,down,down1,up,up1,scrlgray,gray50,gray25,i_fifo," + -
             "i_chr,i_dir,i_blk,i_lnk,i_sock,i_reg,i_exe,rb_frame,"        + -
             "rb_frame1,rb_body,rb_top,rb_dtop,rb_bot,rb_dbot,rb_dot,uph," + -
             "uph1,downh,downh1,fc_left,fc_leftm,fc_mid,fc_midm,fc_right," + -
             "fc_rightm,fc_left1,fc_left1m,fc_right1,fc_right1m,icon,"     + -
             "dial_cw1,dial_cw2,dial_ccw1,dial_ccw2,iconmask,gf1_addh,"    + -
             "gf1_delh,gf1_line,gf1_rst,gf1_spln,gf1_gamma,h_rotl,h_rotr," + -
             "h_sinc,h_sdec,h_flip,cb_check,h_sat,h_desat,root_weave,"     + -
             "cboard50,mb_chk,fliph,flipv,p10,m10"
$!
$   ndx = 0
$APPEND_NEXT:
$   name = F$Element (ndx, ",", bmlist)
$   If name .eqs. "," Then Return
$   ndx = ndx + 1
$   Append [.bitmaps]'name'. []bitmaps.h
$   GoTo APPEND_NEXT
$!
$EXIT:
$   VERIFY = F$Verify (VERIFY)
$   Exit
