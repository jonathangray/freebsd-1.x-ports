all:
	@echo Everything is done by make World

install: World

World:
	@echo Making xview3.2 and clients
	@echo If you get an error message about to many open files you have
	@echo to raise your limit, in tcsh: "limit openfiles 200"
	(cd xview/config; make install)
	(cd xview; xmkmf)
	(cd xview; make Makefiles includes all install install.man)
	(cd xview; make SUBDIRS=clients Makefiles all install install.man)
	tar cvf - share|(cd /usr/X386; tar xf -)
	@echo Make and Installation of xview is complete!
	@echo Don't forget to set OPENWINHOME in your ~/.tcshrc or equiv.

xview32b.tgz: 
	pkg_create -c xview-bin-desc -d README -f xview-bin-list xview32b

xview32l.tgz:
	pkg_create -c xview-lib-desc -d README -f xview-lib-list xview32l

pkg:	World xview32b.tgz xview32l.tgz

clean:
	rm -f xview32b.tgz xview32l.tgz
	(cd xview; make clean)
	(cd xview; make SUBDIRS=clients clean)
	(cd xview; rm -rf build)
	(cd xview; mkdir build)
