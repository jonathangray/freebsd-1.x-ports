PROG=	kermit
CFLAGS+= -I${.CURDIR} -DBSD44 -DCK_CURSES -DDYNAMIC -DTCPSOCKET
SRCS=   ckcmai.c ckucmd.c ckuusr.c ckuus2.c ckuus3.c ckuus4.c ckuus5.c \
        ckuus6.c ckuus7.c ckuusx.c ckuusy.c ckcpro.c ckcfns.c ckcfn2.c \
        ckcfn3.c ckuxla.c ckucon.c ckutio.c ckufio.c ckudia.c ckuscr.c \
        ckcnet.c

# uncomment these for uucp setuid dialout 
#CFLAGS+= -DSETREUID
#BINMODE=4510
#BINOWN= uucp
#BINGRP= dialer

BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man

DPADD=  ${LIBCURSES} ${LIBTERM}
LDADD=  -lcurses -ltermcap

.SUFFIXES: .w

.w.c:
	./wart ${.IMPSRC} ${.TARGET}

wart: ckwart.c
	$(CC) -o wart ${.CURDIR}/ckwart.c


ckcpro.c: ckcpro.w
ckcpro.c: wart

CLEANFILES+= ckcpro.c wart

pkg:	pkg_src pkg_bin

pkg_src: clean pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S kermit_src

pkg_bin: install pkg/+DESC_B pkg/+ONELINE_B pkg/+PLIST_B
	pkg_create -c pkg/+ONELINE_B -d pkg/+DESC_B -f pkg/+PLIST_B kermit_bin

.include <bsd.prog.mk>
