#	Makefile	4.2	83/07/27
#
DESTDIR=

package: pkg_bin

all:
	cd src; make ${MFLAGS}

pkg_bin: install
        pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST cpm_bin

install: all
	cd src; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd man; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean:
	cd src; make ${MFLAGS} clean
