prefix=/usr/gnu
lib=${prefix}/lib/ghostscript

all: ghostscript
	@echo done

ghostscript:
	cd gs; \
	make

install: 
	-mkdir -p ${lib}/fonts
	install -c -o bin -g bin -m 444  fonts/[a-z]* ${lib}/fonts
	cd gs; \
	make install
	-mkdir -p ${prefix}/man/man1
	install -c -o bin -g bin -m 444  gs/gs.1 ${prefix}/man/man1/gs.1

clean:
	cd gs; \
	make clean

package:
	strip /usr/gnu/bin/gs
	pkg_create -v -c bin-desc -d README.FreeBSD -f bin-list ghostscript_bin.tgz
