all: make-octave

make-octave:
	@echo "###############################################################"
	@echo "# Expect a number of warning from make when it tries to fopen #"
	@echo "# dependencs files with extension .d. This is intended behav- #"
	@echo "# iour and the files will be created as needed.               #"
	@echo "###############################################################"
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
