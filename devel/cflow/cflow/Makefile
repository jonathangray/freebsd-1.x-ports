PROG=	cflow
.if exists(${.CURDIR}/obj)
TARG=  ${.CURDIR}/obj/${PROG}
.else
TARG=  ${.CURDIR}/${PROG}
.endif

all:	${TARG}

depend rcsfreeze tags all:
	@echo -n

cleandir: clean
	cd ${.CURDIR}; rm -rf obj;

clean:
	rm -f ${TARG}

install:
	install -c -o bin -g bin -m 555 ${TARG} ${BINDIR}
	install -c -o bin -g bin -m 444 ${TARG}.1 ${MANDIR}

${TARG}: ${.CURDIR}/${PROG}.sh
	cp ${.CURDIR}/${PROG}.sh ${TARG}

.include "../Makefile.inc"
