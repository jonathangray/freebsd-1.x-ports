-------------------------------------------------------------------------
@REM    To compile dvi2xx.c for MSDOS with MSC Rel 6.0:

set MSC=\C600
set INCLUDE=%MSC%\include
set LIB=%MSC%\lib

set CFLAGS=-AL -W2 -Gs -Ox
set LFLAGS=/stack:9000 /EXEPACK

@REM For all version (LJ and 3812)

   cl %CFLAGS% -c findfile.c ;;


@REM  LaserJet+ and  LaserJet II Version:

   cl %CFLAGS% -DLJ2 dvi2xx.c findfile.obj -link %LFLAGS%
   copy dvi2xx.exe dvilj2.exe


@REM  LaserJet II P and LaserJet III and LaserJet 2000 Version:

   cl %CFLAGS% -DLJ2P dvi2xx.c findfile.obj -link %LFLAGS%
   copy dvi2xx.exe dvilj2p.exe

@REM  IBM3812 Version:

   cl %CFLAGS% -DIBM3812 dvi2xx.c findfile.obj -link %LFLAGS%
   copy dvi2xx.exe dvi3812.exe

@REM  LaserJet with 7 bit

   cl %CFLAGS% -DLJ -DSEVENBIT dvi2xx.c findfile.obj -link %LFLAGS%
   copy dvi2xx.exe dvilj.exe


@REM  LaserJet II P with 7 bit and landscape

   cl %CFLAGS% -DLJ2P -DSEVENBIT  dvi2xx.c findfile.obj -link %LFLAGS%
   copy dvi2xx.exe dviljp.exe

   del dvi2xx.exe
-------------------------------------------------------------------------
@REM    To compile dvi2xx.c under MSDOS for OS/2 with MSC Rel 6.0:

set MSC=\C600
set INCLUDE=%MSC%\include
set LIB=%MSC%\lib

set CFLAGS=-AL -W2 -Gs -UMSDOS -DOS2 -Ox -Lp -Fb
set LFLAGS=/stack:9000 /EXEPACK /PMTYPE:VIO

@REM For all version (LJ and 3812)

   cl %CFLAGS% -c findfile.c ;;


@REM LaserJet II Version:

   cl %CFLAGS% -DLJ2  dvi2xx.c findfile.obj -link %LFLAGS%
   markexe -t windowcompat dvi2xx.exe
   copy dvi2xx.exe odvilj2.exe


@REM  LaserJet II P and LaserJet III and LaserJet 2000 Version:

   cl %CFLAGS% -DLJ2P  dvi2xx.c findfile.obj -link %LFLAGS%
   markexe -t windowcompat dvi2xx.exe
   copy dvi2xx.exe odvilj2p.exe


@REM  LaserJet with 7 bit

   cl %CFLAGS% -DLJ -DSEVENBIT dvi2xx.c findfile.obj -link %LFLAGS%
   markexe -t windowcompat dvi2xx.exe
   copy dvi2xx.exe odvilj.exe


@REM  LaserJet II P with 7 bit and landscape

   cl %CFLAGS% -DLJ2P -DSEVENBIT  dvi2xx.c findfile.obj -link %LFLAGS%
   markexe -t windowcompat dvi2xx.exe
   copy dvi2xx.exe odviljp.exe


@REM  IBM3812 Version:

   cl %CFLAGS% -DIBM3812 dvi2xx.c findfile.obj -link %LFLAGS%
   markexe -t windowcompat dvi2xx.exe
   copy dvi2xx.exe odvi3812.exe

   del dvi2xx.exe

-------------------------------------------------------------------------
# makefile for the dvi-to-(3812 || lj + || lj II || lj IIp ) filter.
#
CFLAGS=-O -s -Dunix
BINDIR=/usr/local/bin
MANDIR=/usr/man
MANSEC=1
CC=cc

# under Ultrix 4.1 use:
CFLAGS=-O1 -s -Dunix

# for gcc you might wish to set the following flags
CC=gcc
CFLAGS=-O2 -Dunix -ansi -pedantic

# under AIX 3.1 add the following line:
#LFLAGS=-lbsd

OBJS=findlj.o find3812.o
HFILES=commands.h config.h
PROGRAMS=dvilj2p dvilj2 dvi3812 dvilj dviljp

all: $(PROGRAMS) dvi2xx.txt

dvi3812: dvi2xx.c find3812.o $(HFILES)
	$(CC) ${CFLAGS} -DIBM3812 -o $@ dvi2xx.c find3812.o ${LFLAGS}

dvilj2p: dvi2xx.c findlj.o  $(HFILES)
	$(CC) ${CFLAGS} -DLJ2P -o $@ dvi2xx.c findlj.o  ${LFLAGS}

dvilj2: dvi2xx.c findlj.o  $(HFILES)
	$(CC) ${CFLAGS} -DLJ2 -o $@ dvi2xx.c findlj.o ${LFLAGS}

dvilj: dvi2xx.c findlj.o  $(HFILES)
	$(CC) ${CFLAGS} -DLJ -DSEVENBIT -o $@ dvi2xx.c findlj.o  ${LFLAGS}

dviljp: dvi2xx.c findlj.o  $(HFILES)
	$(CC) ${CFLAGS} -DLJ2P -DSEVENBIT -o $@ dvi2xx.c findlj.o  ${LFLAGS}

findlj.o: findfile.c config.h
	$(CC) ${CFLAGS} -DLJ -c findfile.c
	mv findfile.o $@

find3812.o: findfile.c config.h
	$(CC) ${CFLAGS} -DIBM3812 -c findfile.c
	mv findfile.o $@

clean:
	rm -f core $(OBJS) $(PROGRAMS)

DISTFILES = \
	./dvi2xx.c ./commands.h ./config.h ./findfile.c ./Makefile \
	./testpage.tex ./dvi2xx.history ./dvi2xx.man ./graybox.sty \
	./dvi2xx.make ./dvi2xx.051 ./README \
	./MakeTeXPK ./lj3-filter ./printcap.sample ./README.vms \
	./sample2.tex ./2up.sty ./README.dell-unix ./dvi2xx.tex

shar: 
	shar ${DISTFILES} > dvi2xx.shar


tar:
	pdtar zcvf dvi2xx.tar.Z ${DISTFILES}
	shar dvi2xx.tar.Z > dvi2xx.shar

testfiles:
	./dvilj2 -eftable.lj2 ftable
	./dvilj2p -eftable.lj2p ftable
	./dvi3812 ftab2


install:
	@for f in $(PROGRAMS); do \
		echo installing $$f in $(BINDIR); \
		install -c $$f $(BINDIR); \
	done
	cp dvi2xx.man dvi2xx.$(MANSEC)
	install dvi2xx.$(MANSEC) $(MANDIR)/man$(MANSEC)

dvi2xx.txt: dvi2xx.man
	-nroff -man dvi2xx.man | col -b > dvi2xx.txt






