#Top level ZSH makefile

SUBDIR=src


package:	pkg_bin

pkg_bin: install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST zsh_bin

.include <bsd.subdir.mk>
