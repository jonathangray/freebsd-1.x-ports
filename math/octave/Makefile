all: octave

octave:
	cd octave; \
	configure; \
	gmake

install: octave
	cd octave; \
	gmake install
clean:
	cd octave; \
	gmake clean

pkg:	all install pkg/+ONELINE pkg/+PLIST
	strip /usr/gnu/bin/octave
	pkg_create -v -c +ONELINE -d README.FreeBSD -f +PLIST octave_bin
