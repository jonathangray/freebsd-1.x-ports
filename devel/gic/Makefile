TK_LIBRARY = /usr/local/lib/tk
#
all install:
	cp dot-gicrc ${HOME}/.gicrc
	cp gic.tcl gic-start.tcl ModalBoxes.tcl FSBox.tcl list_lib.tcl parselog.nawk cvs_log.tcl cvs_diff.tcl ${TK_LIBRARY}
	chmod a+r ${TK_LIBRARY}/gic.tcl ${TK_LIBRARY}/gic-start.tcl ${TK_LIBRARY}/ModalBoxes.tcl ${TK_LIBRARY}/FSBox.tcl ${TK_LIBRARY}/list_lib.tcl ${TK_LIBRARY}/parselog.nawk ${TK_LIBRARY}/cvs_log.tcl ${TK_LIBRARY}/cvs_diff.tcl
	echo 'auto_mkindex $$tk_library *.tcl ; exit' | wish
	@echo "#!/bin/sh" > gic
	@echo "wish -f " ${TK_LIBRARY}/gic-start.tcl >> gic
	@chmod +x gic
	@mv gic /usr/local/bin

clean:
	rm -f gic

pkg:	pkg_src pkg_bin

pkg_src: pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S pkg/+REQ_S clean
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S -r pkg/+REQ_S gic_src

pkg_bin: pkg/+DESC_B pkg/+ONELINE_B pkg/+PLIST_B pkg/+REQ_B pkg/+INST_B all
	pkg_create -c pkg/+ONELINE_B -d pkg/+DESC_B -f pkg/+PLIST_B -r pkg/+REQ_B -i pkg/+INST_B gic_bin
