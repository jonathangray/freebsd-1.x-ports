PROG=   unzip 
CFLAGS+= -DBSD
SRCS=   unzip.c envargs.c explode.c extract.c file_io.c inflate.c \
        mapname.c match.c misc.c unreduce.c unshrink.c 

MAN1= unzip.1 funzip.1 zipinfo.1

BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man

FOBJS=	funzip.o inflate.o
IOBJS=	zipinfo.o envargs_.o match_.o misc_.o

CLEANFILES=funzip.o zipinfo.o envargs_.o match_.o misc_.o funzip zipinfo

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	   ${.CURDIR}/funzip ${.CURDIR}/zipinfo ${DESTDIR}${BINDIR}

all: funzip zipinfo

funzip: ${FOBJS} 
	${CC} ${LDFLAGS} -o ${.TARGET} ${FOBJS}

zipinfo: ${IOBJS}
	${CC} ${LDFLAGS} -o ${.TARGET} ${IOBJS}

zipinfo.o: zipinfo.c
	${CC} -c -DZIPINFO ${CFLAGS} ${.CURDIR}/zipinfo.c -o ${.TARGET}

envargs_.o: envargs.c
	${CC} -c -DZIPINFO ${CFLAGS} ${.CURDIR}/envargs.c -o ${.TARGET}

match_.o: match.c
	${CC} -c -DZIPINFO ${CFLAGS} ${.CURDIR}/match.c -o ${.TARGET}

misc_.o: misc.c
	${CC} -c -DZIPINFO ${CFLAGS} ${.CURDIR}/misc.c -o ${.TARGET}


.include <bsd.prog.mk>

