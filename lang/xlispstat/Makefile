##############################################################################
# Directories
# XLISPLIB is where the xlisp executable, the .lsp files to be loaded on 
# startup and the examples are to be kept.
# BINDIR is where the xlispstat shell script is to be put.
#
# NOTE: if XLISPLIB is not empty it MUST end with a /

XLISPLIB=/usr/local/lib/xlispstat/
BINDIR=/usr/local/bin

##############################################################################
##############################################################################
# Graphics system. Uncomment one of the following choices:

#GRAPHSYS = GNUPLOT
#GRAPHSYS = SUNVIEW
GRAPHSYS = X11WINDOWS

##############################################################################
##############################################################################
# Sun Operating System Version. Only used for SUNVIEW graphics system.
# Use SUN3X for sun OS 3.X and SUN4X for sun OS 4.X under SunView;

#SUNOS = SUN3X
#SUNOS = SUN4X

##############################################################################
##############################################################################
# User Compiler Flags
# Add a -I directive if the X11 include files are not in a standard place
# For Sun's OpenLook, for example, add -I/usr/openwin/include

#UCFLAGS = -O -f68881
#ULDFLAGS = -f68881
UCFLAGS = -O3 -I/usr/X386/include
ULDFLAGS =
CC=cc
LD=ld

##############################################################################
##############################################################################
# Foreign function call flag. If this flag is defined you should also
# define a machine chosen from one of the directories in the machines
# directory. Look at the README file in the machine directory for your
# machine - you may have to add some additional compiler or ld flags
# to make dynamic loading work.
#
# If you do not want to use foreign function calling define the machine
# as `generic'

FOREIGN_FLAG = -DFOREIGNCALL
MACHINE = freebsd

##############################################################################
##############################################################################
# Additional libraries and files to load at compile time

EXTRALIBS=
EXTRAOBJS=

##############################################################################
##############################################################################
# Directory to search for X11 libraries (libX11.a, etc.)

X11LIBDIR=/usr/X386/lib

###############################################################################
###############################################################################
###                                                                         ###
###                      DO NOT EDIT BELOW THIS LINE                        ###
###                                                                         ###
###############################################################################
###############################################################################

CFLAGS = -DUNIX -D${GRAPHSYS} ${UCFLAGS} ${FOREIGN_FLAG}
LIBS = ${EXTRALIBS} -lm
LDFLAGS = ${ULDFLAGS}

OSOBJS = xsdynload.o xssystem.o

XLISPOBJS = xlbfun.o xlcont.o xldbug.o xldmem.o \
	xleval.o xlfio.o xlftab.o xlglob.o xlimage.o xlinit.o xlio.o \
	xlisp.o xljump.o xllist.o xlpp.o xlprin.o \
	xlread.o xlstr.o xlsubr.o xlsym.o xlsys.o xlstruct.o

COMMONOBJS = common.o commonarrays.o commonmath.o complex.o 

STATOBJS = basics.o compound.o distribs.o ddistribs.o \
	hardwareobs.o matrices1.o matrices2.o math.o objectinit.o \
	objects.o optimize.o statfloat.o sortdata.o statinit.o \
	statistics.o uni.o utilities.o utilities2.o gamln.o splines.o \
	kernel.o lowess.o cfft.o

DISTOBJS = betabase.o gammabase.o nor.o ppnd.o studentbase.o bivnor.o

LINALGOBJS = linalg.o cholesky.o ludecomp.o qrdecomp.o rcondest.o svdecomp.o \
	 makerotation.o

GRAFOBJS = xsiview2.o xsiviewintrn.o xsnewplots.o \
	xsgraphics.o xsiview3.o xsiviewwin.o xsiview.o \
	xsiviewwin2.o menus.o dialogs.o iviewdata.o iviewscale.o iview.o \
	iviewintrn.o xshistogram.o xsnamelist.o xsscatmat.o xsspin.o \
	xsscatterpl.o windows.o stmem.o graphics.o myplot.o term.o \
	postscript.o

BAYESOBJS = minimize.o functions.o derivatives.o xsbayes.o


BASICOBJS = ${OSOBJS} ${XLISPOBJS} ${COMMONOBJS} ${STATOBJS} ${DISTOBJS} \
	${LINALGOBJS} ${GRAFOBJS} ${BAYESOBJS}

OBJS = ${BASICOBJS} ${EXTRAOBJS}

GNUPLOTOBJS = unixstuff.o dummygraph.o

SUNOBJS = sunstuff.o sungraphwin.o sunresizebr.o sunwindows.o \
	sundialogs.o sunmenus.o

X11WINDOWSOBJS = X11BSDstuff.o X11graph.o X11menus.o X11dialogs.o \
	X11buttons.o X11text.o X11toggle.o X11choice.o X11slider.o \
	X11listitem.o X11resizebr.o X11scroll.o

LISPFILES = init.lsp common.lsp help.lsp objects.lsp statistics.lsp \
	dialogs.lsp graphics.lsp graphics2.lsp graphics3.lsp regression.lsp \
	oneway.lsp nonlin.lsp maximize.lsp bayes.lsp step.lsp autoload.lsp \
	menubar.lsp xlisp.help


xlispstat: Makefile xlisp setup.shell
	./setup.shell xlispstat ${XLISPLIB}
	chmod a+x xlispstat

pkg:	install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST xlispstat_bin

install: installexecs installlisp

installexecs: xlispstat xlisp
	cp xlispstat ${BINDIR}
	-mkdir ${XLISPLIB}
	cp xlisp ${XLISPLIB}
	
installlisp:
	-mkdir ${XLISPLIB}
	cp ${LISPFILES} ${XLISPLIB}
	-mkdir ${XLISPLIB}/Data
	-mkdir ${XLISPLIB}/Examples
	cp Data/* ${XLISPLIB}/Data
	cp Examples/* ${XLISPLIB}/Examples


installX11sun: X11sun installlisp
	cp xlispstat ${BINDIR}
	cp sunxlisp X11xlisp ${XLISPLIB}

xlisp:	${GRAPHSYS}xlisp

GNUPLOTxlisp:	${OBJS} ${GNUPLOTOBJS}
	${CC} ${LDFLAGS} -o xlisp ${OBJS} ${GNUPLOTOBJS} ${LIBS}
	touch GNUPLOTxlisp

SUNVIEWxlisp:	${OBJS} ${SUNOBJS}
	${CC} ${LDFLAGS} -o xlisp ${OBJS} ${SUNOBJS} \
	-lsuntool -lsunwindow -lpixrect ${LIBS}
	touch SUNVIEWxlisp

X11WINDOWSxlisp:	${OBJS} ${X11WINDOWSOBJS}
	${CC} ${LDFLAGS} -o xlisp ${OBJS} ${X11WINDOWSOBJS} \
	-L${X11LIBDIR} -lX11 ${LIBS}
	touch X11WINDOWSxlisp

X11sun:	sun X11
	setup.shell xlispstat ${XLISPLIB} both
	chmod a+x xlispstat
	
sun:
	touch Makefile
	make xlisp GRAPHSYS=SUNVIEW
	mv xlisp sunxlisp

X11:
	touch Makefile
	make xlisp GRAPHSYS=X11WINDOWS
	mv xlisp X11xlisp
	
clean:
	rm -f core *.o *~

cleanall: clean
	rm -f xlisp sunxlisp X11xlisp xlispstat


# More dependencies are needed here
sunwindows.o: sunwindows.c xlisp.h windows.h
sungraphwin.o: sungraphwin.c StGWWindow.h stmem.h
sundialogs.o: sundialogs.c xlisp.h dialogs.h

sunstuff.o: sunstuff.c xlisp.h version.h
	${CC} -c ${CFLAGS} -D${SUNOS} sunstuff.c

unixstuff.o: unixstuff.c xlisp.h version.h Makefile
graphics.o: graphics.c gnuplot.h xlisp.h
myplot.o: myplot.c gnuplot.h xlisp.h
term.o: term.c gnuplot.h xlisp.h

X11graph.o: X11graph.c xlisp.h StGWWindow.h StX11options.h
X11BSDstuff.o: X11BSDstuff.c xlisp.h version.h
X11dialogs.o: X11dialogs.c xlisp.h dialogs.h
X11buttons.o: X11buttons.c xlisp.h dialogs.h
X11text.o: X11text.c xlisp.h dialogs.h
X11toggle.o: X11toggle.c xlisp.h dialogs.h
X11choice.o: X11choice.c xlisp.h dialogs.h
X11slider.o: X11slider.c xlisp.h dialogs.h
X11listitem.o: X11listitem.c xlisp.h dialogs.h

xlisp.h: xldmem.h
xlisp.o: xlisp.c xlisp.h Makefile
xlftab.o: xlftab.c osdefs.h osptrs.h Makefile
statinit.o: statinit.c Makefile
iviewdata.o: iviewdata.c xlisp.h StGWWindow.h stmem.h
objectinit.o: objectinit.c xlisp.h Makefile
xsdynload.o: xsdynload.c xlisp.h foreign.h Makefile

foreign.h: machines/${MACHINE}/foreign.h Makefile
	cp machines/${MACHINE}/foreign.h .
