# /usr/ports/editor/Makefile.inc 0.1	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

.if !defined(AVOID)
#	AVOID as make currently goes recursive
SUBDIR += emacs
#	make: don't know how to make install. Stop
.endif

SUBDIR += uemacs
#	 Place the cmd/bpage.cmd cmd/cpage.cmd cmd/cua.cmd cmd/dev.cmd cmd/ehelp.cmd \
#		cmd/epage.cmd cmd/error.cmd cmd/filter.cmd cmd/lpage.cmd cmd/mdi.cmd \
#		cmd/mewin.cmd cmd/newpage.cmd cmd/opage.cmd cmd/ppage.cmd cmd/shell.cmd \
#		cmd/utod.cmd cmd/wpage.cmd \
#	 files somewhere in your path, where
#	 they can be read by uemacs, and copy cmd/emacs.rc to
#	 ~/.emacsrc in all users home directories who wish to
#	 use MicroEmacs.

SUBDIR += vim

.include <bsd.subdir_ports.mk>

# End of Makefile.
