PROG=	kermit
CFLAGS+= -I${.CURDIR} -DBSD44 -DCK_CURSES -DDYNAMIC -DTCPSOCKET -DNOCOTFMC
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

package:	pkg_bin

pkg_bin: install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST kermit_bin

.include <bsd.prog.mk>
