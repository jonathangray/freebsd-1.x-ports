.SUFFIXES:
.SUFFIXES: .sc .o

OBJS =	ezd.o pps.o commands.o ginfo.o display.o window.o view.o drawing.o \
	graphic.o rectangle.o line.o text.o arc.o psdraw.o events.o \
	interact.o stringinput.o popupmenu.o buttons.o slider.o textree.o \
	jtextree.o textdrawing.o mouseedit.o editcolor.o struct.o match.o \
	quilt.o transpbuttons.o

SCFLAGS = -O

#SCXL = /udir/bartlett/alpha/bin/scxl.a
#SC = /udir/bartlett/alpha/bin/schemetoc/libsc.a
#X11 = -non_shared -lX11 -ldnet_stub

#SCXL = /udir/bartlett/mips/bin/scxl.a
#SC = /udir/bartlett/mips/bin/schemetoc/libsc.a
#X11 = -lX11

SCXL = /usr/local/lib/schemetoc/scxl.a
SC = /usr/local/lib/schemetoc/libsc.a
X11 = -lX11

.sc.o:
	scc -c ${SCFLAGS} $*.sc

all:	ezd libezd.a # DECstation-ezd.o

ezd:	${SCXL} ${OBJS} ezdmain.o
	scc -o ezd ${SCFLAGS} ezdmain.o ${OBJS} ${SCXL} ${X11}

${SCXL}:
	@echo "Making required Scheme->C library first."
	@(cd ../../lang/schemetoc; make all install)

libezd.a:	${OBJS}
	rm -f libezd.a
	ar q libezd.a ${OBJS}
	ranlib libezd.a

DECstation-ezd.o:	${OBJS}
	ld -o DECstation-ezd.o -r ezdmain.o ${OBJS} ${SCXL} ${SC}

ezd-for-DECstation:
	cc -o ezd DECstation-ezd.o ${X11} -lm

clean:
	rm -f ${OBJS} ezdmain.o DECstation-ezd.o

package: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST ezd_bin

noprogs:
	rm -f ezd libezd.a

tartape:
	tar cvf ezd.tar README DECstation-ezd.o *.sc *.sch makefile \
		doc/ezd.1 doc/ezd.psf \
		examples/clock.c examples/clock.sc examples/dragon.sc \
		examples/puzzle.sc examples/puzzle_in_c.c examples/xc.sc \
		examples/makefile
	rm -f ezd.tar.Z
	compress ezd.tar

install:
	install -c libezd.a /usr/local/lib
	install -cs ezd /usr/local/bin
	install -c -m 644 doc/ezd.1 /usr/local/man/man1
