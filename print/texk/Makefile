all:
	cp Makefile.in web2c*/
	cd web2c*; \
	if [ ! -f web2c*/Makefile ]; then \
		./configure; \
	fi; \
	make

install:
	cd web2c*; make install

clean:
	cd web2c*; make clean

realclean:
	cd web2c*; make realclean
