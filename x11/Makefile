# /usr/ports/x11/Makefile V2.3	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += ghostscript
SUBDIR += ghostview
SUBDIR += jpeg
SUBDIR += pbmplus
SUBDIR += ups
SUBDIR += urt
SUBDIR += tk #	depends on tcl

.if !defined(AVOID)
G_SUBDIR += xview
#	(cd xview3.2; xmkmf)
#	mv Makefile Makefile.bak
#	imake -DUseInstalled -I/usr/X386/lib/X11/config
#	In file included from /usr/X386/lib/X11/config/Imake.tmpl:1004, from :3:
#	./Imakefile:1: XView.tmpl: No such file or directory
#	imake: Exit code 33. Stop.
#	make World 	ar: notifydata.o: Too many open files
#	even make clean blows up :-(
.endif

SUBDIR += xlock

.if !defined(AVOID)
SUBDIR += xxgdb
#	calldbx.c:77: termio.h: No such file or directory
#	In file included from defs.h:67, from global.h:83, from calldbx.c:84:
#	/usr/include/sys/param.h:164: warning: `MAXPATHLEN' redefined
#	global.h:72: warning: this is the location of the previous definition
.endif

SUBDIR += xv

.include <bsd.subdir_ports.mk>

# End of Makefile.
