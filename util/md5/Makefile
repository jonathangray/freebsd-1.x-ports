
PROG=	md5
SRCS=	mddriver.c md5c.c
BINDIR= /usr/local/bin
MANDIR= /usr/local/man/man

# If this fails, we shouldn't install.
beforeinstall:
	./md5 -x | diff - ${.CURDIR}/test.result

pkg: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST md5

.include <bsd.prog.mk>
