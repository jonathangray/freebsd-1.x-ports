#********************************************************************
# lindner
# 3.11
# 1993/08/19 20:31:54
# /home/mudhoney/GopherSrc/CVS/gopher+/Makefile,v
# Exp
#
# Paul Lindner, University of Minnesota CIS.
#
# Copyright 1991, 1992 by the Regents of the University of Minnesota
# see the file "Copyright" in the distribution for conditions of use.
#********************************************************************
# MODULE: Makefile
# Makefile for entire gopher distribution.
#*********************************************************************
# Revision History:
# Makefile,v
# Revision 3.11  1993/08/19  20:31:54  lindner
# minor fixes for quicktar
#
# Revision 3.10  1993/08/17  21:58:11  lindner
# Fix for dependencies, all
#
# Revision 3.9  1993/08/12  06:34:23  lindner
# Added quicktar..
#
# Revision 3.8  1993/08/05  22:22:40  lindner
# warning message for archive
#
# Revision 3.7  1993/07/27  05:32:28  lindner
# added gophfilt and zip stuff
#
# Revision 3.6  1993/07/21  16:15:58  lindner
# Added tags target, install for shared libraries
#
# Revision 3.5  1993/04/15  22:00:52  lindner
# Move to looking for ui for WAIS stuff
#
# Revision 3.4  1993/03/18  23:11:15  lindner
# 1.2b3 release
#
# Revision 3.3  1993/02/19  21:41:58  lindner
# Fix to automatically copy Makefile.config.dist when making a distribution.
#
# Revision 3.2  1993/02/19  21:24:24  lindner
# Mods for CVS
#
# Revision 3.1.1.1  1993/02/11  18:02:49  lindner
# Gopher+1.2beta release
#
# Revision 1.3  1992/12/29  23:30:22  lindner
# Removed all references to fanout and mindexd, it's in gopherd now..
#
# Revision 1.2  1992/12/11  19:32:00  lindner
# make tar now uses gnu tar, ignores RCS directorys and symbolic links
#
# Revision 1.1  1992/12/11  19:01:43  lindner
# Gopher1.1 Release
#
#********************************************************************/


include Makefile.config

all: objects server client gopherfilter
	@echo "Welcome to Gopher"

server: gopherd/gopherd
client: gopher/gopher
gopherfilter: gophfilt/gophfilt
objects: object/libgopher.a

gopherd/gopherd: object/libgopher.a gopherd/*.c gopherd/*.h conf.h
	@echo "Making server"
	@if [ -f /sdmach -a -d ./ui ]; then \
		(echo "Making server with NeXT and WAIS searching";\
		 cd gopherd; $(MAKE) $(MFLAGS) "SEARCH=-DNEXTSEARCH -DWAISSEARCH" nextwais);\
	elif [ -f /sdmach ]; then \
		(echo "Making server with NeXT searching";\
		 cd gopherd; $(MAKE) $(MFLAGS) "SEARCH=-DNEXTSEARCH" next); \
	elif [ -d ./ui ]; then \
		(echo "Making server with WAIS searching";\
		 cd gopherd; $(MAKE) $(MFLAGS) "SEARCH=-DWAISSEARCH" wais); \
	else (echo "Making data server"; cd gopherd; $(MAKE) $(MFLAGS) none) \
	fi

# gopher is dependant on conf.h - (as are other things)
gopher/gopher: object/libgopher.a conf.h gopher/*c gopher/*h
	@echo "Making client"
	(cd gopher; $(MAKE) $(MFLAGS))

gophfilt/gophfilt: object/libgopher.a conf.h gophfilt/*c
	@echo "Making gophfilt"
	(cd gophfilt; $(MAKE) $(MFLAGS))

object/libgopher.a: object/*c object/*h
	@echo "Making Objects"
	(cd object; $(MAKE) $(MFLAGS))

install:
	(cd object;   $(MAKE) $(MFLAGS) install);
	(cd gopher;   $(MAKE) $(MFLAGS) install);
	(cd gopherd;  $(MAKE) $(MFLAGS) install);
	(cd gophfilt; $(MAKE) $(MFLAGS) install);
	(cd doc;      $(MAKE) $(MFLAGS) install)

clean:
	(cd gopher; 	$(MAKE) $(MFLAGS) clean)
	(cd gopherd; 	$(MAKE) $(MFLAGS) clean)
	(cd object; 	$(MAKE) $(MFLAGS) clean)
	(cd gophfilt;   $(MAKE) $(MFLAGS) clean)
	(cd doc; 	$(MAKE) $(MFLAGS) clean)
	-rm -f "examples/Sample Directory/wais-index/index."*

spotless: clean
	-rm -f *~

archive: spotless
	-rm  -f ir ui bin MANIFEST
	touch MANIFEST
	cp Makefile.config.dist Makefile.config
	HERE=`basename $$PWD`; \
	echo $$HERE ; \
        cd .. ; \
	find $$HERE -type f -print |grep -v '.o$$' |grep -v '~$$' |grep -v 'CVS' >$$HERE/MANIFEST ; \
	/usr/gnu/bin/tar  -T $$HERE/MANIFEST -czvf $$HERE.tar.Z ;\
	zip -@ $$HERE.zip <$$HERE/MANIFEST
	@echo "Did you remember to update the patchlevel.h Paul?..."
	@echo "Hmmm...  Hmmmm...."

quicktar:
	HERE=`basename $$PWD`; \
        cd .. ; \
	/usr/gnu/bin/tar  -T $$HERE/MANIFEST -czvf $$HERE.tar.Z ;

tags:
	etags `find . -print | egrep '\.[ch]$$'`
