# /usr/ports/net/Makefile Thu Feb 17 17:26:04 MEZ 1994	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += dgd
SUBDIR += ncftp
G_SUBDIR += gopher
SUBDIR += sup
SUBDIR += tcl-dp

.if !defined(AVOID) & defined(WAIT_OK)
.if exists(DPWISH_BINARY)
SUBDIR += zircon #			depends on tcl, tk & dpwish
#	cd lib; echo 'auto_mkindex . *.tcl; exit' | /usr/local/bin/dpwish
#	cd: can't cd to lib
.endif
.endif

.include <bsd.subdir_ports.mk>

# End of Makefile.
