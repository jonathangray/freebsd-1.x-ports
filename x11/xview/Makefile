all:
	@echo Everything is done by make World

install: World

config:
	(cd xview/config; $(MAKE) install)
	(cd xview; xmkmf)
	(cd xview; $(MAKE) Makefiles)
	(cd xview; $(MAKE) SUBDIRS=clients Makefiles)
	touch stamp-config

World:	config
	@echo Making xview3.2 and clients
	rm -f xview/lib/libxview/libxview.a
	(cd xview; $(MAKE) includes all install install.man)
	(cd xview; $(MAKE) SUBDIRS=clients all install install.man)
	tar cvf - share|(cd /usr/X386; tar xf -)
	@echo Make and Installation of xview is complete!
	@echo Don't forget to set OPENWINHOME in your ~/.tcshrc or equiv.

package:	World pkg/+ONELINE pkg/+DESC pkg/+PLIST
	pkg_create -c pkg/+ONELINE  -d pkg/+DESC -f pkg/+PLIST xview_bin

clean:
	rm -f xview_bin.tgz
	if [ -f stamp-config ] ; then \
		(cd xview; $(MAKE) clean) ; \
		(cd xview; $(MAKE) SUBDIRS=clients clean) ; \
		rm stamp-config ; \
	fi ;
	(cd xview; rm -rf build)
	(cd xview; mkdir build)
