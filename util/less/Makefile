SUBDIR=	less lesskey

pkg: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST less_bin

.include <bsd.subdir.mk>
