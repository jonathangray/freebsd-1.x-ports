PROG=	kermit
CFLAGS+= -DBSD44 -DCK_CURSES -DDYNAMIC -DTCPSOCKET -I${.CURDIR}
SRCS=	ckcmai.c ckucmd.c ckuusr.c ckuus2.c ckuus3.c ckuus4.c ckuus5.c \
	ckuus6.c ckuus7.c ckuusx.c ckuusy.c ckcpro.c ckcfns.c ckcfn2.c \
	ckcfn3.c ckuxla.c ckucon.c ckutio.c ckufio.c ckudia.c ckuscr.c \
	ckcnet.c

.if !defined(${BINDIR})
BINDIR=/usr/local/bin
.endif
.if !defined(${MANDIR})
MANDIR=/usr/local/man/man
.endif

DPADD=	${LIBCURSES} ${LIBTERM}
LDADD=	-lcurses -ltermcap
CLEANFILES= ckcpro.c wart

.if exists(${.CURDIR}/obj)
OBJ=obj
.else
OBJ=.
.endif

ckcpro.c: ckcpro.w  
	${CC} ${CFLAGS} -o ${.CURDIR}/${OBJ}/wart ${.CURDIR}/ckwart.c
	${.CURDIR}/${OBJ}/wart ${.CURDIR}/ckcpro.w ${.TARGET}

.include <bsd.prog.mk>
