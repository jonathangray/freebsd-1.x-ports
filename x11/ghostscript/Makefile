prefix=/usr/local
lib=${prefix}/lib/ghostscript

all: ghostscript
	@echo done

ghostscript:
	cd gs; \
	make

install: ghostscript
	@if whoami | egrep 'root|bin'; then : ; \
	else \
		echo "Install as either user root or bin"; \
		exit 1; \
	fi;
	-mkdir -p ${lib}/fonts
	cp -R fonts  ${lib}/
	cd gs; \
	make install; \
	cp gs.1 ${prefix}/man/man1/gs.1

clean:
	cd gs; \
	make clean
