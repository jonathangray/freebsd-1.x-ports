SUBDIR=lha unarj unzip zip zoo


package:	pkg_bin

pkg_bin: install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST archivers_bin

.include <bsd.subdir.mk>
