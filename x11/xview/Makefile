SRCDIR = /xview
World:
	@echo Making xview3.2 and clients
	@echo If you get an error message about to many open files you have
	@echo to raise your limit, in tcsh: "limit openfiles 200"
	(cd xview3.2; make includes all install install.man)
	(cd xview3.2; make SUBDIRS=clients all install install.man)
	tar cvf - share|(cd /usr/X386; tar xf -)
	@echo Make and Installation of xview is complete!
	@echo Don't forget to set OPENWINHOME in your ~/.tcshrc or equiv.
xview32b.tgz:
	(cd /usr/X386; $(SRCDIR)/pkg_create -v -c $(SRCDIR)/xview-bin-desc -d $(SRCDIR)/README -f $(SRCDIR)/xview-bin-list xview32b.tgz)
	mv /usr/X386/xview32b.tgz .
xview32l.tgz:
	(cd /usr/X386; $(SRCDIR)/pkg_create -v -c $(SRCDIR)/xview-lib-desc -d $(SRCDIR)/README -f $(SRCDIR)/xview-lib-list xview32l.tgz)
	mv /usr/X386/xview32l.tgz .


