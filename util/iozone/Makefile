#	$Id: Makefile,v 1.3 1994/02/17 17:22:51 rgrimes Exp $

PROG=	iozone
#SRCS=	iozone.c
NOMAN=	YES
BINDIR?= /usr/local/bin
CFLAGS+= -DUSE_FSYNC

pkg: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST iozone_bin

.include <bsd.prog.mk>
