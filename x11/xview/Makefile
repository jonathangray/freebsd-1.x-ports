SRCDIR = /xview
World:
	@echo Making xview3.2 and clients
	@echo If you get an error message about to many open files you have
	@echo to raise your limit, in tcsh: "limit openfiles 200"
	(cd xview3.2; make includes all install install.man)
	(cd xview3.2; make SUBDIRS=clients all install install.man)
	(cd xview3.2/config; make install)
	tar cvf - share|(cd /usr/X386; tar xf -)
	@echo Make and Installation of xview is complete!
	@echo Don't forget to set OPENWINHOME in your ~/.tcshrc or equiv.
xview32b.tgz:
	(cd /usr/X386; pkg_create -v -c $(SRCDIR)/xview-bin-desc -d $(SRCDIR)/README -f $(SRCDIR)/xview-bin-list xview32b.tgz)
	mv /usr/X386/xview32b.tgz .
xview32l.tgz:
	(cd /usr/X386; pkg_create -v -c $(SRCDIR)/xview-lib-desc -d $(SRCDIR)/README -f $(SRCDIR)/xview-lib-list xview32l.tgz)
	mv /usr/X386/xview32l.tgz .
xview32s.tgz:	clean
	(cd ..; pkg_create -v -c $(SRCDIR)/xview-src-desc -d $(SRCDIR)/README -f $(SRCDIR)/xview-src-list xview32s.tgz)
	mv ../xview32s.tgz .
clean:
	rm -f xview32b.tgz xview32l.tgz xview32s.tgz
	(cd xview3.2; make clean)
	(cd xview3.2; make SUBDIRS=clients clean)
	(cd xview3.2; rm -rf build)
	(cd xview3.2; mkdir build)
