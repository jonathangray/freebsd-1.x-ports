PROG=zoo
CFLAGS+= -DBSD4_3 -DANSI_HDRS  -DBIG_MEM -DNDEBUG

BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man

SRCS= addbfcrc.c addfname.c basename.c comment.c crcdefs.c getfile.c lzc.c \
	lzd.c machine.c makelist.c misc.c misc2.c nextfile.c needed.c \
	options.c parse.c portable.c prterror.c version.c zoo.c zooadd.c \
	zooadd2.c zoodel.c zooext.c zoofilt.c zoolist.c zoopack.c io.c \
	lzh.c maketbl.c maketree.c huf.c encode.c decode.c

FOBJS= fiz.o addbfcrc.o portable.o crcdefs.o

MAN1= zoo.1 fiz.1
CLEANFILES=fiz.o fiz

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	   ${.CURDIR}/fiz ${DESTDIR}${BINDIR}

all: fiz

fiz: ${FOBJS}
	${CC} ${LDFLAGS} -o ${.TARGET} ${FOBJS}

.include <bsd.prog.mk>
