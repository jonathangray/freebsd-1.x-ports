all: make-octave

make-octave:
	cd octave; \
	./configure; \
	gmake

install: make-octave
	cd octave; \
	gmake install
clean:
	cd octave; \
	gmake clean

pkg:	all install pkg/+ONELINE pkg/+PLIST
	strip /usr/gnu/bin/octave
	pkg_create -v -c pkg/+ONELINE -d README.FreeBSD -f pkg/+PLIST octave_bin
