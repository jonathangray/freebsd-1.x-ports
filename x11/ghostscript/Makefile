prefix=/usr/gnu
lib=${prefix}/lib/ghostscript

all: ghostscript
	@echo done

ghostscript:
	cd gs; \
	make

install: 
	-mkdir -p ${lib}/fonts
	cp -R fonts  ${lib}/
	cd gs; \
	make install
	-mkdir -p ${prefix}/man/man1
	cp gs.1 ${prefix}/man/man1/gs.1

clean:
	cd gs; \
	make clean
