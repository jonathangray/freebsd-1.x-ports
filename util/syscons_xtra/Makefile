SUBDIR = utils dgl

obj:
	@echo -n

pkg: pkg_bin

pkg_bin: install
        pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST syscons_xtra_bin

.include <bsd.subdir.mk>
