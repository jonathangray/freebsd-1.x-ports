# Makefile generated by imake - do not edit!
# $XConsortium: imake.c,v 1.65 91/07/25 17:50:17 rws Exp $

# -------------------------------------------------------------------------
# Makefile generated from "Imake.tmpl" and <Imakefile>
# $XFree86: mit/config/Imake.tmpl,v 1.17 1993/06/03 15:26:36 dawes Exp $
# $XConsortium: Imake.tmpl,v 1.139 91/09/16 08:52:48 rws Exp $
#
# Platform-specific parameters may be set in the appropriate <vendor>.cf
# configuration files.  Site-specific parameters should be set in the file
# site.def.  Full rebuilds are recommended if any parameters are changed.
#
# If your C preprocessor does not define any unique symbols, you will need
# to set BOOTSTRAPCFLAGS when rebuilding imake (usually when doing
# "make World" the first time).
#

# -------------------------------------------------------------------------
# site-specific configuration parameters that need to come before
# the platform-specific parameters - edit site.def to change

# $XFree86: mit/config/site.def,v 1.65 1993/06/04 16:02:47 dawes Exp $
# site:  $XConsortium: site.def,v 1.2 91/07/30 20:26:44 rws Exp $

# -------------------------------------------------------------------------
# platform-specific configuration parameters - edit x386.cf to change

# $XFree86: mit/config/x386.cf,v 1.90 1993/06/04 16:02:50 dawes Exp $
# platform:  $XConsortium: x386.cf,v 1.7 91/08/16 19:30:10 gildea Exp $

# -------------------------------------------------------------------------
# XFree86 version definition
# $XFree86: mit/config/xf86_vers.def,v 1.5 1993/06/01 09:12:47 dawes Exp $

# -------------------------------------------------------------------------
# XFree86 version: 1300
# -------------------------------------------------------------------------

#  --------------------------------------------------------------------
#  X386Conf.tmpl  -- configuration parameters for X386
#  This file is automatically generated -- DO NOT EDIT
#  --------------------------------------------------------------------

         X386SRC = $(SERVERSRC)/ddx/x386
    VGADRIVERSRC = $(X386SRC)/vga256/drivers
   VGA2DRIVERSRC = $(X386SRC)/vga2/drivers
   HGA2DRIVERSRC = $(X386SRC)/hga2/drivers
     RENDERERSRC = $(FONTSRC)/lib/font
 RENDERERCONFOBJ = $(RENDERERSRC)/fontfile/rendererConf.o
RENDERERCONFSUBDIR = $(RENDERERSRC)/fontfile
         VGAOBJS =                    $(VGADRIVERSRC)/et4000/et4000.o                    $(VGADRIVERSRC)/et3000/et3000.o                    $(VGADRIVERSRC)/pvga1/pvga1.o                    $(VGADRIVERSRC)/gvga/gvga.o                    $(VGADRIVERSRC)/ati/ati.o                    $(VGADRIVERSRC)/tvga8900/tvga8900.o                    $(VGADRIVERSRC)/cirrus/cirrus.o                    $(VGADRIVERSRC)/ncr/ncr.o                    $(VGADRIVERSRC)/compaq/compaq.o

      VGASUBDIRS =                    et4000                    et3000                    pvga1                    gvga                    ati                    tvga8900                    cirrus                    ncr                    compaq

        VGA2OBJS =                    $(VGA2DRIVERSRC)/et4000/et4000.o                    $(VGA2DRIVERSRC)/et3000/et3000.o                    $(VGA2DRIVERSRC)/pvga1/pvga1.o                    $(VGA2DRIVERSRC)/gvga/gvga.o                    $(VGA2DRIVERSRC)/tvga8900/tvga8900.o                    $(VGA2DRIVERSRC)/ncr/ncr.o                    $(VGA2DRIVERSRC)/compaq/compaq.o                    $(VGA2DRIVERSRC)/generic/generic.o

     VGA2SUBDIRS =                    et4000                    et3000                    pvga1                    gvga                    tvga8900                    ncr                    compaq                    generic

        HGA2OBJS =                    $(HGA2DRIVERSRC)/hga6845/hga6845.o

     HGA2SUBDIRS =                    hga6845

    RENDERERLIBS =                    $(RENDERERSRC)/Speedo/libSpeedo.a                    $(RENDERERSRC)/Type1/libType1.a

 RENDERERSUBDIRS =                    Speedo                    Type1

     VGA2CONFOBJ = $(X386SRC)/vga2/vga2Conf.o
  VGA2CONFSUBDIR = $(X386SRC)/vga2
       VGA2BUILD = -DBUILD_VGA2

     HGA2CONFOBJ = $(X386SRC)/hga2/hga2Conf.o
  HGA2CONFSUBDIR = $(X386SRC)/hga2
       HGA2BUILD = -DBUILD_HGA2

   X386MCONFOBJS = $(VGA2CONFOBJ) $(HGA2CONFOBJ)
X386MCONFSUBDIRS = $(VGA2CONFSUBDIR) $(HGA2CONFSUBDIR)

      VGACONFOBJ = $(X386SRC)/vga256/vga256Conf.o
   VGACONFSUBDIR = $(X386SRC)/vga256
        VGABUILD = -DBUILD_VGA256

    X386CONFOBJS = $(VGACONFOBJ)
 X386CONFSUBDIRS = $(VGACONFSUBDIR)

 X386SCREENFLAGS = $(VGA2BUILD) $(HGA2BUILD) $(VGABUILD)

# operating system:  		386BSD

# -------------------------------------------------------------------------
# site-specific configuration parameters that go after
# the platform-specific parameters - edit site.def to change

# $XFree86: mit/config/site.def,v 1.65 1993/06/04 16:02:47 dawes Exp $
# site:  $XConsortium: site.def,v 1.2 91/07/30 20:26:44 rws Exp $

            SHELL = 	/bin/sh

              TOP = .
      CURRENT_DIR = .

               AR = ar clq
  BOOTSTRAPCFLAGS =
               CC = 		gcc
               AS = as

             YACC = yacc

         COMPRESS = compress
              CPP = 		/usr/libexec/cpp $(STD_CPP_DEFINES)
    PREPROCESSCMD = 		gcc -E $(STD_CPP_DEFINES)
          INSTALL = install
               LD = ld
             LINT = lint
      LINTLIBFLAG = -C
         LINTOPTS = -axz
               LN = ln -s
             MAKE = make
               MV = mv
               CP = cp

           RANLIB = ranlib
  RANLIBINSTFLAGS =

               RM = rm -f
            TROFF = psroff
         MSMACROS = -ms
              TBL = tbl
              EQN = eqn
     STD_INCLUDES =
  STD_CPP_DEFINES = -traditional
      STD_DEFINES =
 EXTRA_LOAD_FLAGS =
  EXTRA_LIBRARIES =
     OS_LIBRARIES =
             TAGS = ctags

    PROTO_DEFINES =

     INSTPGMFLAGS = -s

     INSTBINFLAGS = -m 0755
     INSTUIDFLAGS = -m 4755
     INSTLIBFLAGS = -m 0644
     INSTINCFLAGS = -m 0444
     INSTMANFLAGS = -m 0444
     INSTDATFLAGS = -m 0444
    INSTKMEMFLAGS = -m 4755

      PROJECTROOT = 	/usr/X386

     TOP_INCLUDES = -I$(TOP)

      CDEBUGFLAGS = -O2
        CCOPTIONS = -m486
    ANSICCOPTIONS =

      ALLINCLUDES = $(INCLUDES) $(EXTRA_INCLUDES) $(TOP_INCLUDES) $(STD_INCLUDES)
       ALLDEFINES = $(ALLINCLUDES) $(STD_DEFINES) $(EXTRA_DEFINES) $(PROTO_DEFINES) $(DEFINES)
           CFLAGS = $(ANSICCOPTIONS) $(CDEBUGFLAGS) $(CCOPTIONS) $(ALLDEFINES)
        LINTFLAGS = $(LINTOPTS) -DLINT $(ALLDEFINES)

           LDLIBS = -L$(USRLIBDIR) $(OS_LIBRARIES) $(SYS_LIBRARIES) $(EXTRA_LIBRARIES)

        LDOPTIONS = $(ANSICCOPTIONS) $(CDEBUGFLAGS) $(CCOPTIONS) $(LOCAL_LDFLAGS)

   LDCOMBINEFLAGS = -X -r
      DEPENDFLAGS =

        MACROFILE = x386.cf
           RM_CMD = $(RM) *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ *.a .emacs_* tags TAGS make.log MakeOut

    IMAKE_DEFINES =

         IRULESRC = $(CONFIGSRC)
        IMAKE_CMD = $(NEWTOP)$(IMAKE) -I$(NEWTOP)$(IRULESRC) $(IMAKE_DEFINES)

     ICONFIGFILES = $(IRULESRC)/Imake.tmpl $(IRULESRC)/Imake.rules 			$(IRULESRC)/Project.tmpl $(IRULESRC)/site.def 			$(IRULESRC)/$(MACROFILE) $(EXTRA_ICONFIGFILES)

# -------------------------------------------------------------------------
# X Window System Build Parameters
# $XFree86: mit/config/Project.tmpl,v 1.13 1993/03/27 03:32:45 dawes Exp $
# $XConsortium: Project.tmpl,v 1.138.1.1 92/11/11 09:49:19 rws Exp $

_percentC_ = %C

# -------------------------------------------------------------------------
# X Window System make variables; this need to be coordinated with rules

          PATHSEP = /
        USRLIBDIR = 	/usr/X386/lib
           BINDIR = 	/usr/X386/bin
          INCROOT = 	/usr/X386/include
     BUILDINCROOT = $(TOP)
      BUILDINCDIR = $(BUILDINCROOT)/X11
      BUILDINCTOP = ..
           INCDIR = $(INCROOT)/X11
           ADMDIR = /usr/adm
           LIBDIR = $(USRLIBDIR)/X11
        CONFIGDIR = $(LIBDIR)/config
       LINTLIBDIR = $(USRLIBDIR)/lint

          FONTDIR = $(LIBDIR)/fonts
         XINITDIR = $(LIBDIR)/xinit
           XDMDIR = $(LIBDIR)/xdm
           TWMDIR = $(LIBDIR)/twm
          MANPATH = 	/usr/X386/man
    MANSOURCEPATH = 	$(MANPATH)/man
        MANSUFFIX = 1
     LIBMANSUFFIX = 3
           MANDIR = 	$(MANSOURCEPATH)1
        LIBMANDIR = $(MANSOURCEPATH)3
           NLSDIR = $(LIBDIR)/nls
        PEXAPIDIR = $(LIBDIR)/PEX
      XAPPLOADDIR = $(LIBDIR)/app-defaults
       FONTCFLAGS = -t
       LINKKITDIR = 	$(USRLIBDIR)/Server

     INSTAPPFLAGS = $(INSTDATFLAGS)

            IMAKE = $(IMAKESRC)/imake
           DEPEND = $(DEPENDSRC)/makedepend
              RGB = $(RGBSRC)/rgb

            FONTC = $(BDFTOPCFSRC)/bdftopcf

        MKFONTDIR = $(MKFONTDIRSRC)/mkfontdir
        MKDIRHIER = 	/bin/sh $(SCRIPTSRC)/mkdirhier.sh

        CONFIGSRC = $(TOP)/config
       DOCUTILSRC = $(TOP)/doc/util
        CLIENTSRC = $(TOP)/clients
          DEMOSRC = $(TOP)/demos
           LIBSRC = $(TOP)/lib
          FONTSRC = $(TOP)/fonts
       INCLUDESRC = $(TOP)/X11
        SERVERSRC = $(TOP)/server
          UTILSRC = $(TOP)/util
        SCRIPTSRC = $(UTILSRC)/scripts
       EXAMPLESRC = $(TOP)/examples
       CONTRIBSRC = $(TOP)/../contrib
           DOCSRC = $(TOP)/doc
           RGBSRC = $(TOP)/rgb
        DEPENDSRC = $(UTILSRC)/makedepend
         IMAKESRC = $(CONFIGSRC)
         XAUTHSRC = $(LIBSRC)/Xau
          XLIBSRC = $(LIBSRC)/X
           XMUSRC = $(LIBSRC)/Xmu
       TOOLKITSRC = $(LIBSRC)/Xt
       AWIDGETSRC = $(LIBSRC)/Xaw
       OLDXLIBSRC = $(LIBSRC)/oldX
      XDMCPLIBSRC = $(LIBSRC)/Xdmcp
      BDFTOSNFSRC = $(FONTSRC)/bdftosnf
      BDFTOSNFSRC = $(FONTSRC)/clients/bdftosnf
      BDFTOPCFSRC = $(FONTSRC)/clients/bdftopcf
     MKFONTDIRSRC = $(FONTSRC)/clients/mkfontdir
         FSLIBSRC = $(FONTSRC)/lib/fs
    FONTSERVERSRC = $(FONTSRC)/server
     EXTENSIONSRC = $(TOP)/extensions
         XILIBSRC = $(EXTENSIONSRC)/lib/xinput
        PEXLIBSRC = $(EXTENSIONSRC)/lib/PEXlib
      PHIGSLIBSRC = $(EXTENSIONSRC)/lib/PEX

  DEPEXTENSIONLIB = $(EXTENSIONSRC)/lib/libXext.a
     EXTENSIONLIB = 			  $(DEPEXTENSIONLIB)

          DEPXLIB = $(DEPEXTENSIONLIB) $(XLIBSRC)/libX11.a
             XLIB = $(EXTENSIONLIB) 			 $(XLIBSRC)/libX11.a

      DEPXAUTHLIB = $(XAUTHSRC)/libXau.a
         XAUTHLIB = 			 $(DEPXAUTHLIB)
      DEPXDMCPLIB = $(XDMCPLIBSRC)/libXdmcp.a
         XDMCPLIB = 			 $(DEPXDMCPLIB)

        DEPXMULIB = $(XMUSRC)/libXmu.a
           XMULIB = 			 $(DEPXMULIB)

       DEPOLDXLIB = $(OLDXLIBSRC)/liboldX.a
          OLDXLIB = 			 $(DEPOLDXLIB)

      DEPXTOOLLIB = $(TOOLKITSRC)/libXt.a
         XTOOLLIB = 			 $(DEPXTOOLLIB)

        DEPXAWLIB = $(AWIDGETSRC)/libXaw.a
           XAWLIB = 			 $(DEPXAWLIB)

        DEPXILIB = $(XILIBSRC)/libXi.a
           XILIB = 			 $(DEPXILIB)

       DEPPEXLIB = $(PEXLIBSRC)/libPEX5.a
          PEXLIB = 			 $(DEPPEXLIB)

        DEPPHIGSLIB = $(PHIGSLIBSRC)/libphigs.a
           PHIGSLIB = 			 $(DEPPHIGSLIB)

       DEPXBSDLIB = $(LIBSRC)/Berk/libXbsd.a
          XBSDLIB = 			 $(DEPXBSDLIB)

 LINTEXTENSIONLIB = $(EXTENSIONSRC)/lib/llib-lXext.ln
         LINTXLIB = $(XLIBSRC)/llib-lX11.ln
          LINTXMU = $(XMUSRC)/llib-lXmu.ln
        LINTXTOOL = $(TOOLKITSRC)/llib-lXt.ln
          LINTXAW = $(AWIDGETSRC)/llib-lXaw.ln
           LINTXI = $(XILIBSRC)/llib-lXi.ln
          LINTPEX = $(PEXLIBSRC)/llib-lPEX5.ln
        LINTPHIGS = $(PHIGSLIBSRC)/llib-lphigs.ln

          DEPLIBS = $(LOCAL_LIBRARIES)

         DEPLIBS1 = $(DEPLIBS)
         DEPLIBS2 = $(DEPLIBS)
         DEPLIBS3 = $(DEPLIBS)

# -------------------------------------------------------------------------
# Imake rules for building libraries, programs, scripts, and data files
# $XFree86: mit/config/Imake.rules,v 1.9 1993/03/23 12:56:27 dawes Exp $
# rules:  $XConsortium: Imake.rules,v 1.123 91/09/16 20:12:16 rws Exp $

# -------------------------------------------------------------------------
# start of Imakefile

###########################################################################
# @(#)XView.cf	1.10 09 Feb 1992 SMI
# XView.cf for XView release 3.0.  Specify site preferences here.

###########################################################################
# @(#)XView.tmpl	1.15 12 May 1993 SMI
# XView.tmpl for XView release 3.2.  Template to add additional config
# rules to those supplied with X Version 11 release 5.

           XV_CFLAGS.optimized = -DOPTIMIZED
           XV_CFLAGS.shared =

                DEFINES = $(LOCALE_DEFINES) $(MMAP_DEFINES) $(XV_ALLOC_DEFINES) 			  $(XV_STRDUP_DEFINES) $(XV_CFLAGS.optimized) $(XV_CFLAGS.shared)

              XVDESTDIR = /usr/X386
           LIBRARY_DEST = $(XVDESTDIR)/lib
      LINT_LIBRARY_DEST = $(XVDESTDIR)/lib/lint
                 BINDIR = $(XVDESTDIR)/bin
                MANPATH = $(XVDESTDIR)/man
            HEADER_DEST = $(XVDESTDIR)/include
               SRC_DEST = $(XVDESTDIR)/share/src/xview
               DOC_DEST = $(XVDESTDIR)/share/doc/xview

               XVLIBSRC = $(TOP)/lib
              HELP_DEST = $(LIBRARY_DEST)/help
    LIBRARY_DEST_EXTRAS = $(LIBRARY_DEST)
       TEXT_EXTRAS_MENU = $(LIBRARY_DEST)/.text_extras_menu
                     AR = ar ruv
                  CHMOD = chmod

                DEPXLIB =

            BUILDINCDIR = $(TOP)/build
            BUILDINCTOP = ../../..

                 INCDIR = /include

               XVPUBINC = xview
               XVPRIINC = xview_private
               OLPUBINC = olgx
               OLPRIINC = olgx_private
               PSPUBINC = xvps
               PSPRIINC = xvps_private
               IMAGEINC = images
              BITMAPINC = bitmaps
         PIXRECT_INCDIR = $(INCDIR)/pixrect
           XVPUB_INCDIR = $(INCDIR)/$(XVPUBINC)
           XVPRI_INCDIR = $(INCDIR)/$(XVPRIINC)
           OLPUB_INCDIR = $(INCDIR)/$(OLPUBINC)
           OLPRI_INCDIR = $(INCDIR)/$(OLPRIINC)
           PSPUB_INCDIR = $(INCDIR)/$(PSPUBINC)
           PSPRI_INCDIR = $(INCDIR)/$(PSPRIINC)
           IMAGE_INCDIR = $(INCDIR)/$(IMAGEINC)
          BITMAP_INCDIR = $(INCDIR)/$(BITMAPINC)
    PIXRECT_HEADER_DEST = $(HEADER_DEST)/pixrect
      XVPUB_HEADER_DEST = $(HEADER_DEST)/$(XVPUBINC)
      XVPRI_HEADER_DEST = $(HEADER_DEST)/$(XVPRIINC)
      OLPUB_HEADER_DEST = $(HEADER_DEST)/$(OLPUBINC)
      OLPRI_HEADER_DEST = $(HEADER_DEST)/$(OLPRIINC)
      PSPUB_HEADER_DEST = $(HEADER_DEST)/$(PSPUBINC)
      PSPRI_HEADER_DEST = $(HEADER_DEST)/$(PSPRIINC)
            IMAGES_DEST = $(HEADER_DEST)/$(IMAGEINC)
            BITMAP_DEST = $(HEADER_DEST)/$(BITMAPINC)

                LINTDIR = $(LINT_LIBRARY_DEST)

            DEPXVIEWLIB = $(XVLIBSRC)/libxview/libxview.a
               XVIEWLIB = 			 -L$(LIBRARY_DEST) -lxview

            DEPOLGXLIB = $(XVLIBSRC)/libolgx/libolgx.a
               OLGXLIB = 			 -L$(LIBRARY_DEST) -lolgx

            DEPXVPSLIB = $(XVLIBSRC)/libxvps/libxvps.a
               XVPSLIB = 			 -L$(LIBRARY_DEST) -lxvps

             XLIB = $(EXTENSIONLIB) 			 -lX11

             SYSV_CLIENT_LIB =
             XVCLIENTSYSDEFINES = -DSUNOS41

        XVCLIENTI18NDEFINES = -DOW_I18N_L3
        XVCLIENTMISCLIB =
        XVCLIENTINCLUDES =

        XVCLIENTDEFINES = $(XVCLIENTSYSDEFINES) $(XVCLIENTI18NDEFINES)

###########################################################################
# @(#)XView.rules	1.8 12 May 1993 SMI
# XView.rules for XView release 3.0.  Imake rules needed by distribution.

# config directory
# @(#)Imakefile	1.9 28 Jun 1993 SMI

XVFILES = XView.cf XView.lib XView.obj XView.rules XView.tmpl XView.prog
CFFILES = $(XVFILES) $(HEADERS)

install:: $(CFFILES)
	@if [ -d $(DESTDIR)$(CONFIGDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(CONFIGDIR)); fi
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in $(CFFILES); do \
	(set -x; $(INSTALL) -c $(INSTDATFLAGS) $$i $(DESTDIR)$(CONFIGDIR)); \
	done

depend::

includes::
	echo "no includes in $(CURRENT_DIR)"

clean::

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

Makefile:: $(IMAKE)

$(IMAKE):
	@(cd $(IMAKESRC); if [ -f Makefile ]; then \
	echo "checking $@ in $(IMAKESRC) first..."; $(MAKE) all; else \
	echo "bootstrapping $@ from Makefile.ini in $(IMAKESRC) first..."; \
	$(MAKE) -f Makefile.ini BOOTSTRAPCFLAGS="$(BOOTSTRAPCFLAGS)"; fi; \
	echo "okay, continuing in $(CURRENT_DIR)")

Makefile::
	-@if [ -f Makefile ]; then set -x; \
	$(RM) Makefile.bak; $(MV) Makefile Makefile.bak; \
	else exit 0; fi
	$(IMAKE_CMD) -DTOPDIR=$(TOP) -DCURDIR=$(CURRENT_DIR)

tags::
	$(TAGS) -w *.[ch]
	$(TAGS) -xw *.[ch] > TAGS

# -------------------------------------------------------------------------
# empty rules for directories that do not have SUBDIRS - do not edit

install::
	@echo "install in $(CURRENT_DIR) done"

install.man::
	@echo "install.man in $(CURRENT_DIR) done"

install.linkkit::
	@echo "install.linkkit in $(CURRENT_DIR) done"

Makefiles::

includes::

# -------------------------------------------------------------------------
# dependencies generated by makedepend

