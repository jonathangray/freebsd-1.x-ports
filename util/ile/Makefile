PROG=	ile

MAN1=	ile.1
LDADD+=	-ltermcap
BINDIR?=	/usr/local/bin
MANDIR=	/usr/local/man/man

package: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST ile_bin

.include <bsd.prog.mk>
