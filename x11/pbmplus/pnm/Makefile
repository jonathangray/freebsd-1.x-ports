# Makefile generated by imake - do not edit!
# $XConsortium: imake.c,v 1.65 91/07/25 17:50:17 rws Exp $

# -------------------------------------------------------------------------
# Makefile generated from "Imake.tmpl" and <Imakefile>
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

              TOP = ../.
      CURRENT_DIR = ././pnm

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

   PBMPLUSDIR =
PBMPLUSBINDIR = $(BINDIR)
PBMPLUSMANDIR = /usr/X386/man
PBMPLUSLIBDIR = $(USRLIBDIR)
PBMPLUSINCDIR = $(INCDIR)

       PBMDIR = .././pbm
       PGMDIR = .././pgm
       PPMDIR = .././ppm
       PNMDIR = .././pnm
      TIFFDIR = .././../tiff

       LIBPBM = $(PBMDIR)/libpbm.a
       LIBPGM = $(PGMDIR)/libpgm.a
       LIBPPM = $(PPMDIR)/libppm.a
       LIBPNM = libpnm.a
      LIBTIFF = -ltiff
    DEPLIBPBM = $(PBMDIR)/libpbm.a
    DEPLIBPGM = $(PGMDIR)/libpgm.a
    DEPLIBPPM = $(PPMDIR)/libppm.a
    DEPLIBPNM = libpnm.a
   DEPLIBTIFF = -ltiff

pkg:	all install install.man pkg/+ONELINE pkg/+DESC pkg/+PLIST
	pkg_create -v -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST pbmplus_bin

   CURRENTLIBS = $(LIBTIFF) $(LIBPNM) $(LIBPPM) $(LIBPGM) $(LIBPBM)
CURRENTDEPLIBS = $(DEPLIBTIFF) $(DEPLIBPNM) $(DEPLIBPPM) $(DEPLIBPGM) $(DEPLIBPBM)
     INCLUDES  = -I.. -I$(PBMDIR) -I$(PGMDIR) -I$(PPMDIR) -I$(TIFFDIR)
       DEFINES = -DLIBTIFF
         MERGE = pnmmerge
      TIFFMAN1 = tifftopnm.1 pnmtotiff.1
      TIFFSRCS = tifftopnm.c pnmtotiff.c
      TIFFOBJS = tifftopnm.o pnmtotiff.o
      TIFFBINS = tifftopnm pnmtotiff

          MAN1 = pnmarith.1 pnmcat.1 pnmconvol.1 pnmcrop.1 pnmcut.1 		 pnmdepth.1 pnmenlarge.1 pnmfile.1 pnmflip.1 pnminvert.1                  pnmnoraw.1 pnmpaste.1 pnmscale.1 pnmtile.1 pnmtops.1                  pnmtorast.1 pnmtoxwd.1 rasttopnm.1 xwdtopnm.1 		 pnmgamma.1 pnmrotate.1 pnmshear.1 		 anytopnm.1 pnmindex.1 pnmmargin.1 pnmsmooth.1 		 $(TIFFMAN1)

          MAN3 = libpnm.3
          MAN5 = pnm.5

          SRCS = pnmarith.c pnmcat.c pnmconvol.c pnmcrop.c pnmcut.c                  pnmdepth.c pnmenlarge.c pnmfile.c pnmflip.c pnminvert.c                  pnmnoraw.c pnmpaste.c pnmscale.c pnmtile.c pnmtops.c                  pnmtorast.c pnmtoxwd.c rasttopnm.c xwdtopnm.c 		 pnmgamma.c pnmrotate.c pnmshear.c 		 $(TIFFSRCS)

          OBJS = pnmarith.o pnmcat.o pnmconvol.o pnmcrop.o pnmcut.o                  pnmdepth.o pnmenlarge.o pnmfile.o pnmflip.o pnminvert.o                  pnmnoraw.o pnmpaste.o pnmscale.o pnmtile.o pnmtops.o                  pnmtorast.o pnmtoxwd.o rasttopnm.o xwdtopnm.o 		 pnmgamma.o pnmrotate.o pnmshear.o 		 $(TIFFOBJS)

          BINS = pnmarith pnmcat pnmconvol pnmcrop pnmcut 		 pnmdepth pnmenlarge pnmfile pnmflip pnminvert 		 pnmnoraw pnmpaste pnmscale pnmtile pnmtops 		 pnmtorast pnmtoxwd rasttopnm xwdtopnm 		 pnmgamma pnmrotate pnmshear 		 $(TIFFBINS)

includes:: anytopnm.script pnmindex.script pnmmargin.script pnmsmooth.script

anytopnm.script:
	$(LN) anytopnm anytopnm.script

pnmindex.script:
	$(LN) pnmindex pnmindex.script

pnmmargin.script:
	$(LN) pnmmargin pnmmargin.script

pnmsmooth.script:
	$(LN) pnmsmooth pnmsmooth.script

all:: $(LIBPNM) $(BINS)

depend::
	$(DEPEND) $(DEPENDFLAGS) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SRCS)

pnmarith.o: pnmarith.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmarith:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmcat.o: pnmcat.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmcat:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmconvol.o: pnmconvol.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmconvol:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmcrop.o: pnmcrop.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmcrop:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmcut.o: pnmcut.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmcut:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmdepth.o: pnmdepth.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmdepth:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmenlarge.o: pnmenlarge.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmenlarge:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmfile.o: pnmfile.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmfile:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmflip.o: pnmflip.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmflip:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnminvert.o: pnminvert.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnminvert:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmnoraw.o: pnmnoraw.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmnoraw:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmpaste.o: pnmpaste.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmpaste:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmscale.o: pnmscale.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmscale:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmtile.o: pnmtile.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmtile:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmtops.o: pnmtops.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmtops:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmtorast.o: pnmtorast.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmtorast:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmtoxwd.o: pnmtoxwd.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmtoxwd:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

rasttopnm.o: rasttopnm.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

rasttopnm:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

xwdtopnm.o: xwdtopnm.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

xwdtopnm:: $(MERGE)
	$(RM) $@
	$(LN) $? $@
\

pnmgamma.o: pnmgamma.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmgamma:: $(MERGE)
	$(RM) $@
	$(LN) $? $@
\

pnmrotate.o: pnmrotate.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmrotate:: $(MERGE)
	$(RM) $@
	$(LN) $? $@
\

pnmshear.o: pnmshear.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmshear:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

tifftopnm.o: tifftopnm.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

tifftopnm:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

pnmtotiff.o: pnmtotiff.c
	$(RM) $@
	$(CC) -c $(CFLAGS) "-Dmain=$*_main" $*.c

pnmtotiff:: $(MERGE)
	$(RM) $@
	$(LN) $? $@

.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $(_NOOP_) $*.c

all:: libpnm.a

libpnm.a: libpnm1.o libpnm2.o libpnm3.o libpnm4.o
	$(RM) $@
	$(AR) $@ libpnm1.o libpnm2.o libpnm3.o libpnm4.o
	$(RANLIB) $@

$(MERGE): $(MERGE).o $(OBJS) $(CURRENTDEPLIBS)
	$(RM) $@
	$(CC) -o $@ $(MERGE).o $(OBJS) $(LDOPTIONS) $(CURRENTLIBS) $(LDLIBS) -lm $(EXTRA_LOAD_FLAGS)

clean::
	$(RM) $(MERGE)

install:: $(MERGE)
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)); fi
	$(INSTALL) -c $(INSTPGMFLAGS)  $(MERGE) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)

install:: $(BINS)
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)); fi
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in $(BINS); do \
	(set -x; cd $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR); $(RM) $$i;  \
	$(LN) $(MERGE) $$i); \
	done

install:: anytopnm.script
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)); fi
	$(INSTALL) -c $(INSTBINFLAGS) anytopnm.script $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)/anytopnm

install:: pnmindex.script
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)); fi
	$(INSTALL) -c $(INSTBINFLAGS) pnmindex.script $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)/pnmindex

install:: pnmmargin.script
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)); fi
	$(INSTALL) -c $(INSTBINFLAGS) pnmmargin.script $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)/pnmmargin

install:: pnmsmooth.script
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR) ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)); fi
	$(INSTALL) -c $(INSTBINFLAGS) pnmsmooth.script $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSBINDIR)/pnmsmooth

install.man:: $(MAN1)
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man1 ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man1); fi
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in $(MAN1); do \
	(set -x; $(INSTALL) -c $(INSTMANFLAGS) $$i $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man1); \
	done

install.man:: $(MAN3)
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man3 ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man3); fi
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in $(MAN3); do \
	(set -x; $(INSTALL) -c $(INSTMANFLAGS) $$i $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man3); \
	done

install.man:: $(MAN5)
	@if [ -d $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man5 ]; then set +x; \
	else (set -x; $(MKDIRHIER) $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man5); fi
	@case '${MFLAGS}' in *[i]*) set +e;; esac; \
	for i in $(MAN5); do \
	(set -x; $(INSTALL) -c $(INSTMANFLAGS) $$i $(DESTDIR)$(PBMPLUSDIR)$(PBMPLUSMANDIR)/man5); \
	done

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

