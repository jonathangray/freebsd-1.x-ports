all: octave

octave:
	cd octave-0.74; \
	configure; \
	gmake

install: octave
	cd octave-0.74; \
	gmake install
clean:
	cd octave-0.74; \
	gmake clean

package:
	strip /usr/gnu/bin/octave
	pkg_create -v -c bin-desc -d README.FreeBSD -f bin-list octave_bin.tgz
