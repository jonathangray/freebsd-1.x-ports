PROG=   zip 
CFLAGS+= -DASMV
SRCS= zip.c zipfile.c zipup.c fileio.c util.c globals.c crypt.c deflate.c \
	trees.c bits.c match.S

MAN1=zip.1 zipgrep.1
MLINKS= zip.1 zipnote.1  zip.1 zipsplit.1  zip.1 zipcloak.1

BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man

NOBJS=	zipnote.o zipfile_.o zipup_.o fileio_.o util_.o globals.o

SOBJS=  zipsplit.o zipfile_.o zipup_.o fileio_.o util_.o globals.o

COBJS=	zipcloak.o zipfile_.o zipup_.o fileio_.o util_.o globals.o crypt_.o

CLEANFILES=zipnote.o zipsplit.o zipcloak.o zipfile_.o zipup_.o fileio_.o \
	util_.o crypt_.o zipnote zipsplit zipcloak


.SUFFIXES: .S

.S.o:
	$(CPP) $(.IMPSRC) | $(AS) $(ASFLAGS) -o $(.TARGET) -

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
           ${.CURDIR}/zipgrep ${.CURDIR}/zipnote ${.CURDIR}/zipsplit \
           ${.CURDIR}/zipcloak ${DESTDIR}${BINDIR}

all: zipnote zipsplit zipcloak

zipnote: ${NOBJS} 
	${CC} ${LDFLAGS} -o ${.TARGET} ${NOBJS}

zipsplit: ${SOBJS}
	${CC} ${LDFLAGS} -o ${.TARGET} ${SOBJS}

zipcloak: ${COBJS}
	${CC} ${LDFLAGS} -o ${.TARGET} ${COBJS}

zipinfo.o: zipinfo.c
	${CC} -c -DZIPINFO ${CFLAGS} ${.CURDIR}/zipinfo.c -o ${.TARGET}

zipfile_.o: zipfile.c
	${CC} -c -DUTIL ${CFLAGS} ${.CURDIR}/zipfile.c -o ${.TARGET}

zipup_.o: zipup.c
	${CC} -c -DUTIL ${CFLAGS} ${.CURDIR}/zipup.c -o ${.TARGET}

fileio_.o: fileio.c
	${CC} -c -DUTIL ${CFLAGS} ${.CURDIR}/fileio.c -o ${.TARGET}

util_.o: util.c
	${CC} -c -DUTIL ${CFLAGS} ${.CURDIR}/util.c -o ${.TARGET}

crypt_.o: crypt.c
	${CC} -c -DUTIL ${CFLAGS} ${.CURDIR}/crypt.c -o ${.TARGET}


.include <bsd.prog.mk>

