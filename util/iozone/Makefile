PROG=	iozone
SRCS=	iozone.c
NOMAN=	YES
BINDIR?= /usr/local/bin

pkg: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST iozone_bin

.include <bsd.prog.mk>
