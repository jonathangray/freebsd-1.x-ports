all:
	@echo Everything is done by make World

install: World

config:
	(cd xview/config; $(MAKE) install)
	(cd xview; xmkmf)
	(cd xview; $(MAKE) Makefiles)
	(cd xview; $(MAKE) SUBDIRS=clients Makefiles)

World:	config
	@echo Making xview3.2 and clients
	rm -f xview/lib/libxview/libxview.a
	(cd xview; $(MAKE) includes all install install.man)
	(cd xview; $(MAKE) SUBDIRS=clients all install install.man)
	tar cvf - share|(cd /usr/X386; tar xf -)
	@echo Make and Installation of xview is complete!
	@echo Don't forget to set OPENWINHOME in your ~/.tcshrc or equiv.

xview32b.tgz: 
	pkg_create -c xview-bin-desc -d README -f xview-bin-list xview32b

xview32l.tgz:
	pkg_create -c xview-lib-desc -d README -f xview-lib-list xview32l

pkg:	World xview32b.tgz xview32l.tgz

clean:	config
	rm -f xview32b.tgz xview32l.tgz
	(cd xview; $(MAKE) clean)
	(cd xview; $(MAKE) SUBDIRS=clients clean)
	(cd xview; rm -rf build)
	(cd xview; mkdir build)
