SUBDIR=lha unarj unzip zip zoo


pkg:	pkg_src pkg_bin

pkg_src: clean pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S archivers_src

pkg_bin: install pkg/+DESC_B pkg/+ONELINE_B pkg/+PLIST_B
	pkg_create -c pkg/+ONELINE_B -d pkg/+DESC_B -f pkg/+PLIST_B archivers_bin

.include <bsd.subdir.mk>
