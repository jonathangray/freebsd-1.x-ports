#	$Id: Makefile,v 1.4 1994/04/11 15:03:32 jkh Exp $

PROG=	iozone
#SRCS=	iozone.c
NOMAN=	YES
BINDIR?= /usr/local/bin
CFLAGS+= -DUSE_FSYNC

package: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST iozone_bin

.include <bsd.prog.mk>
