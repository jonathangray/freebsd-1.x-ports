
TK_LIBRARY = /usr/local/lib/tk

LIBFILES = gic.tcl gic-start.tcl ModalBoxes.tcl FSBox.tcl list_lib.tcl \
	parselog.nawk cvs_log.tcl cvs_diff.tcl

all:
	@if [ ! -x tclsh -a ! -x /usr/local/bin/tclsh ] ; then ; \
		echo '"gic" needs tclsh' 1>&2 ; \
		exit 1 ; \
	fi
	@if [ ! -x wish -a ! -x /usr/local/bin/wish ] ; then ; \
		echo '"gic" needs wish' 1>&2 ; \
		exit 1 ; \
	fi
	cp dot-gicrc dot.gicrc
	echo "#!/bin/sh" > gic
	echo "wish -f " ${TK_LIBRARY}/gic-start.tcl >> gic

install: all
	install -c -m 444 -o bin -g bin $(LIBFILES) $(TK_LIBRARY)
	install -c -m 444 -o bin -g bin dot.gicrc /usr/share/skel
	PATH=$$PATH:/usr/local/bin \
		echo "auto_mkindex ${TK_LIBRARY} *.tcl ; exit" | tclsh
	install -c -m 555 -o bin -g bin gic /usr/local/bin

clean:
	rm -f gic dot.gicrc *.tgz

# Not quite perfect.
package:	pkg_bin

pkg_bin: pkg/+DESC pkg/+ONELINE pkg/+PLIST pkg/+REQ pkg/+INST all
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST -r pkg/+REQ \
		-i pkg/+INST gic_bin
