# /usr/ports/util/Makefile Thu Feb 17 17:26:04 MEZ 1994	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += archivers
SUBDIR += expect # requires `tcl' which gets made from ports/lang/Makefile
SUBDIR += ile
SUBDIR += iozone
SUBDIR += less
SUBDIR += mtools
SUBDIR += pkg_install
SUBDIR += sc
SUBDIR += screen
SUBDIR += ss #	/usr/man/manl/ss.l: No such file or directory

.if !defined(AVOID)
SUBDIR += syscons_xtra
#	gcc -pipe -o mkfil -I../kbdmaps -DFIL=<CVS> mkkbdfil.c
#	mkkbdfil.c:22: directory `../kbdmaps/CVS' specified in #include
.endif

SUBDIR += tcx	#	mkdir /var/exec
SUBDIR += top	#	make: don't know how to make machine.c. Stop
SUBDIR += unshar
SUBDIR += wine
SUBDIR += zcrypt

.include <bsd.subdir_ports.mk>

# End of Makefile.
