# /usr/ports/net/Makefile V2.3	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += dgd
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
