#	@(#)Makefile	4.1	(Berkeley)	10/19/82
#
DESTDIR=/usr/local
DEFSFILE=/usr/local/lib/vgrindefs
TEXINPUTS=$(TEXINPUTS_386BSD)

CFLAGS=-O -g -DDEBUG
SOURCES=tfontedpr.c vgrindefs.c regexp.c retest.c
CMDS=tfontedpr retest
OBJS=retest.o regexp.o tfontedpr.o vgrindefs.o

all:	$(CMDS)

tfontedpr: tfontedpr.o vgrindefs.o regexp.o
	cc ${CFLAGS} -o tfontedpr tfontedpr.o vgrindefs.o regexp.o

tfontedpr.o: tfontedpr.c
	cc ${CFLAGS} -DDEFSFILE=\"${DEFSFILE}\" -c tfontedpr.c

retest: retest.o regexp.o
	cc ${CFLAGS} -o retest retest.o regexp.o

install: all
	install -s tfontedpr ${DESTDIR}/lib/tfontedpr
	install -c tgrind.sh ${DESTDIR}/bin/tgrind
	install -c tgrindmac.tex ${TEXINPUTS}/tgrindmac.tex
	install -c vgrindefs.src ${DEFSFILE}

clean:
	rm -f ${CMDS} ${OBJS}
