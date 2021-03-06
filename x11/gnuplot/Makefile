# Generated automatically from Makefile.in by configure.
############################################################
#
# GNUPLOT 3.4 Makefile (Unix X11 support)
# Adapted from makefile.unx by djm@gnu.ai.mit.edu
############################################################

.NOEXPORT:
SHELL = /bin/sh

srcdir = .

prefix = /usr/local
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
datadir = $(prefix)/lib
mandir = $(prefix)/man/man1

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

CC     = gcc
DEFS   =  -DREADLINE=1 -DNOCWDRC=1 -DX11=1 -DOLD_SELECT=1 -DTERMIOS=1 -DGAMMA=gamma -DGETCWD=1 -DMEMCPY=1 -DMEMSET=1 -DXPG3_LOCALE=1 -DUNIX=1 -DNOVFORK=1 -Dunix=1 -I/usr/X386/include
CFLAGS = -O2 -m486 -pipe
LDFLAGS = -s

# Additional devices you can add.
# -DAPOLLO      Apollo Graphics Primitive Resource (window resize after replot)
# -DGPR         Apollo Graphics Primitive Resource (fixed-size window)
# -DCGI         SCO CGI
# -DIRIS4D      IRIS4D series computer
# -DSUN         Sun Microsystems Workstation
# -DUNIXPC      unixpc (ATT 3b1 or ATT 7300)
# -DUNIXPLOT    unixplot
# -DGNUGRAPH    use GNU graphics version of UNIX plot library
#               This can only be used in combination with -DUNIXPLOT
TERMFLAGS = 

#  -lplot if you have -DUNIXPLOT in TERMFLAGS
#  -lsuntool -lsunwindow -lpixrect  if you have -DSUN in TERMFLAGS
#  -lgl_s if -DIRIS4D in TERMFLAGS
#  -lccgi if -DCGI in TERMFLAGS
TERMLIBS = 

LIBS = $(TERMLIBS) -L/usr/X386/lib -lX11 -lm  -lreadline -ltermcap

# These are set to readline.[co] when configure can't find libreadline
RL_C = 
RL_O = 

# Where to send email about bugs and comments (locally)
EMAIL=bug-gnuplot@dartmouth.edu

# Lasergnu is a handy shell script for creating a plot from the
# command line and sending it directly to the printer. It currently
# supports PostScript and imagen printers, and probably would need
# tailoring to your site.
# Use lasergnu_install to install lasergnu.
# Use lasergnu_noinstall to not install lasergnu (default).
LASERGNU = lasergnu_noinstall

############################################################
# -DREADLINE:
#   If READLINE is defined, then command-line editing is supported.
#   Otherwise, your normal terminal editing is all you get.
#   Some machines will not support this, and they will turn this
#   option off (for example, apollos running SR10.2 or SR10.3 and
#   loaded with BSD4.3 instead of SYS5). Note: problems with
#   gnuplot prompts have been noted when this feature is selected
#   on IBM RS/6000 AIX, and compile errors are encountered on
#   Sequent Dynix 3 and Convex OS 9.0.
# -DNOCWDRC:
#   If NOCWDRC is defined, then any .gnuplot in the current directory
#   is not read on startup. This is a security consideration
#   especially for root users ( we recommend you define -DNOCWDRC ).

# -DOLD_SELECT if you have an old 4.2 BSD OS (e.g. Sun OS 3.5) and
#   encounter compile error for missing FD_* macros for select
# -DBSD_TYPES if your system defines FD_* macros for select in
#   <sys/bsdtypes.h>
# -ISC22 to circumvent missing plots past the first on ISC 2.2

#  -DNOVFORK if you're unix and you have don't have vfork()
#  -DMEMSET if you need to use memset() instead of bzero()
#  -DMEMCPY if your bcopy() is called memcpy()
#  -DNOCOPY if you don't have a memcpy() by any name
#  -DGAMMA=foo if your gamma function is called foo(). Apollos have
#    lgamma(3m). The default is -DGAMMA=gamma.
#  -DGETCWD if your unix uses getcwd() instead of getcd()
#    this is needed by HP-UX and Cray Unicos systems.
#  -DULTRIX_KLUDGE if you run X windows on Ultrix and experience the
#    "every other plot" problem.
#  -DCRIPPLED_SELECT if "select errors" are encountered with X. This
#    option is needed on SVR3 platforms with incomplete support for
#    the BSD select() system call
#  -DXPG3_LOCALE if your system uses the 'setlocale' function to
#    support foreign charactersets (currently only ISO8859-1).
#  -Dunix is required to explicitly define "unix" for SCO and IBM
#          RS/6000 running AIX 3.2

#
# You probably don't need to change anything below here.
#

####################################################################
# List of object files except version.o
OBJS = bitmap.o command.o contour.o eval.o graphics.o graph3d.o help.o \
       internal.o misc.o parse.o plot.o $(RL_O) scanner.o \
       setshow.o specfun.o standard.o term.o util.o gnubin.o binary.o

####################################################################
# List of source files
# Used for making shar files, lint, and some dependencies.
DIRS = term demo docs docs/latextut

CSOURCE1 = bf_test.c binary.c command.c setshow.c
CSOURCE2 = help.c gnubin.c graphics.c graph3d.c internal.c
CSOURCE3 = misc.c eval.c parse.c plot.c $(RL_C) scanner.c standard.c
CSOURCE4 = bitmap.c term.c util.c version.c
CSOURCE5 = term/ai.trm term/amiga.trm term/aed.trm term/atari.trm \
	term/bigfig.trm term/cgi.trm term/corel.trm \
	term/djsvga.trm term/dumb.trm \
	term/dxf.trm term/dxy.trm term/debug.trm \
	term/emxvga.trm term/eepic.trm term/epson.trm term/excl.trm \
	term/fig.trm term/grass.trm term/hp26.trm term/hp2648.trm term/hpgl.trm \
	term/hp500c.trm term/hpljii.trm term/metafont.trm \
	term/apollo.trm term/gpr.trm term/hppj.trm term/compact.c
CSOURCE6 = term/impcodes.h term/imagen.trm term/next.trm term/object.h \
	term/iris4d.trm term/kyo.trm term/latex.trm term/mif.trm \
	term/pbm.trm term/pslatex.trm term/gpic.trm
CSOURCE7 = term/post.trm term/pstricks.trm term/qms.trm term/regis.trm \
	term/rgip.trm term/sun.trm \
	term/t410x.trm term/tek.trm term/texdraw.trm term/tgif.h \
	term/tgif.trm term/tpic.trm \
	term/unixpc.trm term/unixplot.trm \
	term/v384.trm term/vws.trm term/x11.trm term/xlib.trm
CSOURCE8 = contour.c specfun.c gplt_x11.c
NEXTSRC  = epsviewe.m epsviewe.h
# not C code, but still needed

DEMOS = demo/1.dat demo/2.dat demo/3.dat demo/contours.dem \
	demo/controls.dem demo/electron.dem demo/glass.dat demo/param.dem \
	demo/polar.dem demo/simple.dem demo/surface1.dem \
	demo/surface2.dem demo/using.dat demo/using.dem demo/world.cor \
	demo/world.dat demo/world.dem \
	demo/err.dat demo/poldat.dem demo/polar.dat demo/errorbar.dem \
	demo/antenna.dat demo/all.dem demo/animate.dem demo/bivariat.dem \
	demo/prob.dem demo/stat.inc demo/prob2.dem demo/random.dem \
	demo/discrete.dem demo/hidden.dem demo/airfoil.dem demo/gnuplot.rot\
	demo/binary.dem demo/spline.dem demo/steps.dem demo/steps.dat \
	demo/multimsh.dem demo/whale.dat demo/hemisphr.dat \
	demo/scatter.dem demo/scatter2.dat demo/singulr.dem demo/klein.dat

ETC = Copyright 0README README.gnu README.ami makefile.unx makefile.vms \
	linkopt.amg makefile.amg makefile.ami linkopt.vms buildvms.com \
	lasergnu makefile.r makefile.nt makefile.g 0FAQ 0BUGS\
	term/README History gnuplot.el intergra.x11 0INSTALL\
	README.3p2 README.3p3 README.3p4 README.pro README.nex README.x11 \
	README.3d README.mf README.win configure configure.in Makefile.in

#BETA files (not standard distribution files)
BETA = BETA
# PC-specific files
PC = corgraph.asm corplot.c header.mac hrcgraph.asm lineproc.mac \
	linkopt.msc makefile.msc makefile.tc makefile.st makefile.djg \
	pcgraph.asm gnuplot.def makefile.286 gnuplot.prj makefile.emx \
	makefile.ztc linkopt.ztc term/fg.trm term/pc.trm
WINDOWS = makefile.win makefile.msw README.win win/wcommon.h \
	win/wgnuplib.c win/wgnuplib.def win/wgnuplib.h win/wgnuplib.rc \
	win/wgnuplot.def win/wgnuplot.hpj win/wgnuplot.mnu win/wgnuplot.rc \
	win/wgraph.c win/winmain.c win/wmenu.c win/wpause.c \
	win/wprinter.c win/wresourc.h win/wtext.c win/wtext.h \
	win/geticon.c docs/doc2rtf.c term/win.trm
OS2 = makefile.os2 os2/makefile os2/dialogs.c os2/dialogs.h os2/gclient.c \
	os2/gnuicon.uue os2/gnupmdrv.c os2/gnupmdrv.def os2/gnupmdrv.h \
	os2/gnupmdrv.itl os2/gnupmdrv.rc os2/print.c docs/doc2ipf.c \
	README.os2 term/pm.trm

# Documentation and help files
DOCS1 = docs/makefile.org docs/README docs/checkdoc.c docs/doc2gih.c \
	docs/doc2hlp.c docs/doc2hlp.com docs/doc2ms.c docs/doc2tex.c \
	docs/gnuplot.1 docs/lasergnu.1 docs/toc_entr.sty docs/doc2info.pl \
	docs/titlepag.ms docs/titlepag.tex docs/makefile.ami \
	docs/doc2rtf.c docs/Makefile.in
DOCS2 = docs/gnuplot.doc docs/gpcard.tex
DOCS3 = docs/latextut/makefile.org docs/latextut/eg1.plt \
	docs/latextut/eg2.plt docs/latextut/eg3.dat docs/latextut/eg3.plt \
	docs/latextut/eg4.plt docs/latextut/eg5.plt docs/latextut/eg6.plt \
	docs/latextut/header.tex docs/latextut/tutorial.tex \
	docs/latextut/linepoin.plt docs/latextut/Makefile.in

#########################################################################

all: gnuplot gnuplot_x11 doc

gnuplot: $(OBJS) version.o
	$(CC) -o $@ $(OBJS) version.o $(LDFLAGS) $(LIBS)

doc:
	( cd docs; $(MAKE) $(MFLAGS) gnuplot.gih )

gnuplot_x11: gplt_x11.o
	$(CC) -o $@ gplt_x11.o $(LDFLAGS) $(LIBS)

check: all demo/binary1

demo/binary1 demo/binary2 demo/binary3: bf_test
	( wd=`pwd`; cd $(srcdir)/demo; $$wd/bf_test )

bf_test: bf_test.o binary.o
	$(CC) -o $@ bf_test.o binary.o $(LDFLAGS) $(LIBS)

################################################################

install: all $(LASERGNU)
	$(INSTALL_PROGRAM) gnuplot $(bindir)/gnuplot
	test ! -f gnuplot_x11 || $(INSTALL_PROGRAM) gnuplot_x11 $(bindir)/gnuplot_x11
	-$(INSTALL_DATA) $(srcdir)/docs/gnuplot.1 $(mandir)/gnuplot.1
	( cd docs; $(MAKE) $(MFLAGS) install datadir=$(datadir) )

lasergnu_install: lasergnu docs/lasergnu.1
	$(INSTALL_PROGRAM) lasergnu $(bindir)/lasergnu
	$(INSTALL_DATA) $(srcdir)/docs/lasergnu.1 $(mandir)/lasergnu.1

lasergnu_noinstall:

uninstall:
	rm -f $(bindir)/gnuplot $(bindir)/gnuplot_x11
	rm -f $(mandir)/gnuplot.1 $(mandir)/lasergnu.1
	( cd docs; $(MAKE) $(MFLAGS) uninstall datadir=$(datadir) )

################################################################
# Dependencies

.c.o:
	$(CC) -c $(CPPFLAGS) $(DEFS) $(CFLAGS) $<

term.o: term.h term.c $(CSOURCE5) $(CSOURCE6) $(CSOURCE7)
	$(CC) -c -I$(srcdir)/term $(TERMFLAGS) $(CPPFLAGS) $(DEFS) $(CFLAGS) $(srcdir)/term.c

version.o:
	$(CC) -c -DCONTACT=\"$(EMAIL)\" $(CPPFLAGS) $(DEFS) $(CFLAGS) $(srcdir)/version.c

$(OBJS): plot.h

command.o: command.c
	$(CC) -c -DHELPFILE=\"$(datadir)/gnuplot.gih\" $(CPPFLAGS) $(DEFS) $(CFLAGS) $(srcdir)/command.c

command.o help.o misc.o: help.h

command.o graphics.o graph3d.o misc.o plot.o setshow.o term.o: setshow.h

bitmap.o term.o: bitmap.h

################################################################

Makefile: Makefile.in config.status
	$(SHELL) config.status
config.status: configure
	$(SHELL) config.status --recheck
configure: configure.in
	cd $(srcdir); autoconf

################################################################
# Miscellaneous targets

SOURCES=plot.h help.h setshow.h bitmap.h term.h $(CSOURCE1) $(CSOURCE2) \
	$(CSOURCE3) $(CSOURCE4) $(CSOURCE5) $(CSOURCE6) $(CSOURCE7)\
	$(CSOURCE8) $(NEXTSRC) $(WINDOWS) $(OS2)

DOCS  = $(DOCS1) $(DOCS2) $(DOCS3)

clean:
	rm -f gnuplot gnuplot_x11 bf_test *.o core
	( cd docs; $(MAKE) $(MFLAGS) clean )
	( cd docs/latextut; $(MAKE) $(MFLAGS) clean )

mostlyclean: clean

distclean: clean
	rm -f Makefile config.status

realclean: distclean
	rm -f TAGS

dist: $(ETC) $(SOURCES) $(PC) $(DEMOS) $(BETA) $(DOCS)
	$(TAR) cvf /tmp/gnuplot.tar $(ETC) $(SOURCES) $(PC)\
	     $(DEMOS) $(BETA) $(DOCS)

package: install pkg/PLIST pkg/DESC pkg/ONELINE pkg/PREREQ
	pkg_create -c pkg/ONELINE -d pkg/DESC -f pkg/PLIST -r pkg/PREREQ \
	  gnuplot_bin
