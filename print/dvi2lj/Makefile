#	@(#)Makefile	5.3 (Berkeley) 12/4/90
BINDIR=/usr/local/bin
MANDIR=/usr/local/man/man
PROG=	dvi2lj
CFLAGS+=-DLJ2P
SRCS=	dvi2lj.c findfile.c

package:	install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST dvi2lj_bin

afterinstall:
	install -c -o bin -g bin -m 555 \
	${.CURDIR}/df_lj ${DESTDIR}${BINDIR}/df_lj
	install -c -o bin -g bin -m 555 \
	${.CURDIR}/df_lj ${DESTDIR}${BINDIR}/gf_lj
	install -c -o bin -g bin -m 555 \
	${.CURDIR}/df_lj ${DESTDIR}${BINDIR}/if_lj

.include <bsd.prog.mk>
