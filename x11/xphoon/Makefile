# Makefile generated by imake - do not edit!
# $XConsortium: imake.c,v 1.65 91/07/25 17:50:17 rws Exp $

# -------------------------------------------------------------------------
# Makefile generated from "Imake.tmpl" and </tmp/IIf.001922>
# $XFree86: mit/config/Imake.tmpl,v 2.6 1993/10/02 07:11:59 dawes Exp $
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

# $XFree86: mit/config/site.def,v 2.23 1993/10/16 17:28:07 dawes Exp $
# site:  $XConsortium: site.def,v 1.2 91/07/30 20:26:44 rws Exp $

# -------------------------------------------------------------------------
# platform-specific configuration parameters - edit x386.cf to change

# $XFree86: mit/config/x386.cf,v 2.28 1993/10/21 15:48:52 dawes Exp $
# platform:  $XConsortium: x386.cf,v 1.7 91/08/16 19:30:10 gildea Exp $

# -------------------------------------------------------------------------
# XFree86 version definition
# $XFree86: mit/config/xf86_vers.def,v 2.7 1993/10/08 16:11:07 dawes Exp $

# -------------------------------------------------------------------------
# XFree86 version: 2000
# -------------------------------------------------------------------------

# $XConsortium: bsdLib.rules,v 1.7 91/12/20 11:19:47 rws Exp $

# operating system:  	FreeBSD

# -------------------------------------------------------------------------
# site-specific configuration parameters that go after
# the platform-specific parameters - edit site.def to change

# $XFree86: mit/config/site.def,v 2.23 1993/10/16 17:28:07 dawes Exp $
# site:  $XConsortium: site.def,v 1.2 91/07/30 20:26:44 rws Exp $

            SHELL = 	/bin/sh

              TOP = .
      CURRENT_DIR = .

               AR = ar clq
  BOOTSTRAPCFLAGS = 	-DFreeBSD
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
            NROFF = nroff
             NEQN = neqn
     STD_INCLUDES =
  STD_CPP_DEFINES = -traditional 		-DFreeBSD
      STD_DEFINES = 		-DFreeBSD
 EXTRA_LOAD_FLAGS =
  EXTRA_LIBRARIES = 		-lgnumalloc
     OS_LIBRARIES =
             TAGS = ctags

    SHAREDCODEDEF =
         SHLIBDEF = -DBSDSHLIB

    PROTO_DEFINES =

     INSTPGMFLAGS = -s

     INSTBINFLAGS = -m 0755
     INSTUIDFLAGS = -m 4755
     INSTLIBFLAGS = -m 0644
     INSTINCFLAGS = -m 0444
     INSTMANFLAGS = -m 0444
     INSTDATFLAGS = -m 0444
    INSTKMEMFLAGS = 	-m 2755 -g kmem

      PROJECTROOT = 	/usr/X386

     TOP_INCLUDES = -I$(INCROOT)

      CDEBUGFLAGS = -O2
        CCOPTIONS = -m486
    ANSICCOPTIONS =

      ALLINCLUDES = $(INCLUDES) $(EXTRA_INCLUDES) $(TOP_INCLUDES) $(STD_INCLUDES)
       ALLDEFINES = $(ALLINCLUDES) $(STD_DEFINES) $(EXTRA_DEFINES) $(PROTO_DEFINES) $(DEFINES)
           CFLAGS = $(ANSICCOPTIONS) $(CDEBUGFLAGS) $(CCOPTIONS) $(ALLDEFINES)
        LINTFLAGS = $(LINTOPTS) -DLINT $(ALLDEFINES)

           LDLIBS = $(OS_LIBRARIES) $(SYS_LIBRARIES) $(EXTRA_LIBRARIES)

        LDOPTIONS = $(ANSICCOPTIONS) $(CDEBUGFLAGS) $(CCOPTIONS) $(LOCAL_LDFLAGS) -L$(USRLIBDIR)

   LDCOMBINEFLAGS = -X -r
      DEPENDFLAGS =

        MACROFILE = x386.cf
           RM_CMD = $(RM) *.CKP *.ln *.BAK *.bak *.o core errs ,* *~ *.a .emacs_* tags TAGS make.log MakeOut

    IMAKE_DEFINES =

         IRULESRC = $(CONFIGDIR)
        IMAKE_CMD = $(IMAKE) -DUseInstalled -I$(IRULESRC) $(IMAKE_DEFINES)

     ICONFIGFILES = $(IRULESRC)/Imake.tmpl $(IRULESRC)/Imake.rules 			$(IRULESRC)/Project.tmpl $(IRULESRC)/site.def 			$(IRULESRC)/$(MACROFILE) $(EXTRA_ICONFIGFILES)

# -------------------------------------------------------------------------
# X Window System Build Parameters
# $XFree86: mit/config/Project.tmpl,v 2.1 1993/09/17 16:10:43 dawes Exp $
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
           ADMDIR = 		/var/log
           LIBDIR = $(USRLIBDIR)/X11
        CONFIGDIR = $(LIBDIR)/config
       LINTLIBDIR = $(USRLIBDIR)/lint

          FONTDIR = $(LIBDIR)/fonts
         XINITDIR = $(LIBDIR)/xinit
           XDMDIR = $(LIBDIR)/xdm
           TWMDIR = $(LIBDIR)/twm
          MANPATH = 	/usr/X386/man
    MANSOURCEPATH = $(MANPATH)/man
        MANSUFFIX = 1x
     LIBMANSUFFIX = 3x
    FILEMANSUFFIX = 5x
           MANDIR = $(MANSOURCEPATH)1
        LIBMANDIR = $(MANSOURCEPATH)3
       FILEMANDIR = $(MANSOURCEPATH)5
           NLSDIR = $(LIBDIR)/nls
        PEXAPIDIR = $(LIBDIR)/PEX
      XAPPLOADDIR = $(LIBDIR)/app-defaults
       FONTCFLAGS = -t
       LINKKITDIR = 	$(USRLIBDIR)/Server

     INSTAPPFLAGS = $(INSTDATFLAGS)

            IMAKE = imake
           DEPEND = makedepend
              RGB = rgb

            FONTC = bdftopcf

        MKFONTDIR = mkfontdir
        MKDIRHIER = 	/bin/sh $(BINDIR)/mkdirhier

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

# $XConsortium: sunLib.tmpl,v 1.14.1.2 92/11/11 09:52.02 rws Exp $

SHLIBLDFLAGS = -Bshareable
PICFLAGS = -fpic

  DEPEXTENSIONLIB =
     EXTENSIONLIB = -lXext

          DEPXLIB = $(DEPEXTENSIONLIB)
             XLIB = $(EXTENSIONLIB) -lX11

        DEPXMULIB = $(USRLIBDIR)/libXmu.so.$(SOXMUREV)
       XMULIBONLY = -lXmu
           XMULIB = -lXmu

       DEPOLDXLIB =
          OLDXLIB = -loldX

      DEPXTOOLLIB = $(USRLIBDIR)/libXt.so.$(SOXTREV)
         XTOOLLIB = -lXt

        DEPXAWLIB = $(USRLIBDIR)/libXaw.so.$(SOXAWREV)
           XAWLIB = -lXaw

        DEPXILIB =
           XILIB = -lXi

        DEPPEXLIB =
           PEXLIB = -lPEX5

        SOXLIBREV = 2.0
          SOXTREV = 2.0
         SOXAWREV = 2.0
        SOOLDXREV = 2.0
         SOXMUREV = 2.0
        SOXEXTREV = 2.0
      SOXINPUTREV = 2.0
         SOPEXREV = 2.0

      DEPXAUTHLIB = $(USRLIBDIR)/libXau.a
         XAUTHLIB = 			 -lXau
      DEPXDMCPLIB = $(USRLIBDIR)/libXdmcp.a
         XDMCPLIB = 			 -lXdmcp

        DEPPHIGSLIB = $(USRLIBDIR)/libphigs.a
           PHIGSLIB = 			 -lphigs

       DEPXBSDLIB = $(USRLIBDIR)/libXbsd.a
          XBSDLIB = 			 -lXbsd

 LINTEXTENSIONLIB = $(LINTLIBDIR)/llib-lXext.ln
         LINTXLIB = $(LINTLIBDIR)/llib-lX11.ln
          LINTXMU = $(LINTLIBDIR)/llib-lXmu.ln
        LINTXTOOL = $(LINTLIBDIR)/llib-lXt.ln
          LINTXAW = $(LINTLIBDIR)/llib-lXaw.ln
           LINTXI = $(LINTLIBDIR)/llib-lXi.ln
          LINTPEX = $(LINTLIBDIR)/llib-lPEX5.ln
        LINTPHIGS = $(LINTLIBDIR)/llib-lphigs.ln

          DEPLIBS = $(DEPXAWLIB) $(DEPXMULIB) $(DEPXTOOLLIB) $(DEPXLIB)

         DEPLIBS1 = $(DEPLIBS)
         DEPLIBS2 = $(DEPLIBS)
         DEPLIBS3 = $(DEPLIBS)

# -------------------------------------------------------------------------
# Imake rules for building libraries, programs, scripts, and data files
# $XFree86: mit/config/Imake.rules,v 2.1 1993/09/29 11:06:00 dawes Exp $
# rules:  $XConsortium: Imake.rules,v 1.123 91/09/16 20:12:16 rws Exp $

# -------------------------------------------------------------------------
# start of Imakefile

# @(#) $Header: /a/cvs/386BSD/ports/x11/xphoon/Makefile,v 1.2 1994/06/09 11:18:40 asami Exp $ (LBL)
#
# Copyright (C) 1988, 1991 by Jef Poskanzer and Craig Leres.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation.  This software is provided "as is" without express or
# implied warranty.

INCLUDES =		-I. -I$(TOP) -I$(TOP)/X11 -I$(TOP)/X11/bitmaps
LOCAL_LIBRARIES =	$(XLIB)
DEPLIBS =		$(DEPXLIB)
LDLIBS =		-lm

SRCS =			xphoon.c moon.c phase.c dtime.c
OBJS =			xphoon.o moon.o phase.o dtime.o

 PROGRAM = xphoon

all:: xphoon

xphoon: $(OBJS) $(DEPLIBS)
	$(RM) $@
	$(CC) -o $@ $(OBJS) $(LDOPTIONS) $(LOCAL_LIBRARIES) $(LDLIBS) $(EXTRA_LOAD_FLAGS)

install:: xphoon
	@if [ -d $(DESTDIR)$(BINDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(BINDIR)); fi
	$(INSTALL) -c $(INSTPGMFLAGS)  xphoon $(DESTDIR)$(BINDIR)

install.man:: xphoon.man
	@if [ -d $(DESTDIR)$(MANDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(MANDIR)); fi
	$(INSTALL) -c $(INSTMANFLAGS) xphoon.man $(DESTDIR)$(MANDIR)/xphoon.$(MANSUFFIX)

depend::
	$(DEPEND) $(DEPENDFLAGS) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SRCS)

lint:
	$(LINT) $(LINTFLAGS) $(SRCS) $(LINTLIBS)
lint1:
	$(LINT) $(LINTFLAGS) $(FILE) $(LINTLIBS)

clean::
	$(RM) $(PROGRAM)

# Make the export sharchive.
shar:		xphoon.shar1 xphoon.shar2 xphoon.shar3 xphoon.shar4

xphoon.shar1:	README Imakefile xphoon.man xphoon.c moon.c phase.c dtime.c tws.h vroot.h
	shar -v -c -p X README Imakefile xphoon.man xphoon.c moon.c phase.c dtime.c tws.h vroot.h  > xphoon.shar1

moon.xbm.Z:	moon.xbm
	compress -c moon.xbm > moon.xbm.Z

moon.xbm.Z.uu:	moon.xbm.Z
	uuencode moon.xbm.Z moon.xbm.Z > moon.xbm.Z.uu

moon.xbm.Z.uuaa: moon.xbm.Z.uu
	split -510 moon.xbm.Z.uu moon.xbm.Z.uu

xphoon.shar2:	moon.xbm.Z.uuaa
	shar -v -c -p X moon.xbm.Z.uuaa > xphoon.shar2

xphoon.shar3:	moon.xbm.Z.uuaa
	shar -v -c -p X moon.xbm.Z.uuab > xphoon.shar3

xphoon.shar4:	moon.xbm.Z.uuaa
	shar -v -c -p X moon.xbm.Z.uuac > xphoon.shar4

# Unpack the compressed, uuencoded, shared bitmaps.
unshar:
	cat moon.xbm.Z.uu?? > moon.xbm.Z.uu
	uudecode moon.xbm.Z.uu
	uncompress moon.xbm.Z

# Make the export tarchive.
tar:		xphoon.tar.Z

xphoon.tar.Z:	xphoon.tar
	rm -f xphoon.tar.Z
	compress xphoon.tar

xphoon.tar:	README Imakefile xphoon.man xphoon.c moon.c phase.c dtime.c tws.h vroot.h moon.xbm
	tar chf xphoon.tar README Imakefile xphoon.man xphoon.c moon.c phase.c dtime.c tws.h vroot.h moon.xbm

# Extra clean target for shar and tar stuff.
clean::
	-rm -f moon.xbm.Z moon.xbm.Z.uu moon.xbm.Z.uu?? xphoon.shar? xphoon.tar.Z
# FreeBSD package entries

package: pkg_bin

pkg_bin: pkg/+DESC pkg/+ONELINE pkg/+PLIST all install install.man
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST xphoon_bin

# make different sized moons

fullmoon:
	if [ -f moonorig.xbm ]; then cp moonorig.xbm moon.xbm; fi
	$(MAKE) xphoon

1280x1024: fullmoon

1024x768:
	reducemoon 0.7
	$(MAKE) xphoon

800x600:
	reducemoon 0.5
	$(MAKE) xphoon

600x480:
	reducemoon 0.3
	$(MAKE) xphoon

# -------------------------------------------------------------------------
# common rules for all Makefiles - do not edit

emptyrule::

clean::
	$(RM_CMD) "#"*

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

