# Don't change this unless you change it in the interactive install too.
LIBDIR=/usr/local/lib/exmh

all:: install
clean::
	@echo -n

install::
	@if [ ! -f /usr/local/bin/wish ]; then \
		echo "You need to have tk installed to use this package."; \
		exit 1; \
	fi
	wish -f exmh.install

package:	pkg_bin

pkg_bin: pkg/+DESC pkg/+ONELINE pkg/+PLIST pkg/+REQ all
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST -r pkg/+REQ exmh_bin
