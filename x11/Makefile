# /usr/ports/x11/Makefile Thu Feb 17 17:26:04 MEZ 1994	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += ghostscript
SUBDIR += ghostview
SUBDIR += jpeg
SUBDIR += pbmplus
SUBDIR += tiff
SUBDIR += tk #	depends on tcl, which is made by lang/Makefile
SUBDIR += blt
#SUBDIR += ups
SUBDIR += urt
SUBDIR += xlock
SUBDIR += xv
SUBDIR += ezd
SUBDIR += xview

.if !defined(AVOID)
SUBDIR += xxgdb
#	calldbx.c:77: termio.h: No such file or directory
#	In file included from defs.h:67, from global.h:83, from calldbx.c:84:
#	/usr/include/sys/param.h:164: warning: `MAXPATHLEN' redefined
#	global.h:72: warning: this is the location of the previous definition
.endif

.include <bsd.subdir_ports.mk>

# End of Makefile.
