SHELL=	/bin/sh

all:
	cp Makefile.in web2c*/
	cd web2c*; \
	if [ ! -f Makefile ]; then \
		prefix=`sed -n '/^prefix *= *\([^ ]*\).*$$/s//\1/p' Makefile.in`; \
		$(SHELL) ./configure --prefix=$$prefix; \
	fi; \
	make

install:
	cd web2c*; make install

package: pkg_bin

pkg_bin:
	pkg_create -t ./inst.XXXXXX -c pkg/+ONELINE -d pkg/+DESC \
	-f pkg/+PLIST texk_bin

clean:
	cd web2c*; make clean

realclean:
	cd web2c*; make realclean
