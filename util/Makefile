# /usr/ports/util/Makefile V2.3	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += archivers
SUBDIR += mtools
SUBDIR += screen
SUBDIR += tcx #	mkdir /var/exec
SUBDIR += zcrypt
SUBDIR += expect # `expect' requires `tcl' solution: make ports directory twice
SUBDIR += pkg_install
SUBDIR += ss #	/usr/man/manl/ss.l: No such file or directory
SUBDIR += top #	make: don't know how to make machine.c. Stop
SUBDIR += less
SUBDIR += sc

.if !defined(AVOID)
SUBDIR += syscons_xtra
#	gcc -pipe -o mkfil -I../kbdmaps -DFIL=<CVS> mkkbdfil.c
#	mkkbdfil.c:22: directory `../kbdmaps/CVS' specified in #include
.endif

SUBDIR += unshar

.include <bsd.subdir_ports.mk>

# End of Makefile.
