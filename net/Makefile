# /usr/ports/net/Makefile.inc 0.1	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

G_SUBDIR += gopher
# "as ${MAKE} -k breaks on gopher with"
# " Makefile, line 63: Need an operator"

SUBDIR += sup
SUBDIR += tcl-dp
SUBDIR += dgd

.if !defined(AVOID) & defined(WAIT_OK)
.if exists(DPWISH_BINARY)
SUBDIR += zircon #			depends on tcl, tk & dpwish
#	cd lib; echo 'auto_mkindex . *.tcl; exit' | /usr/local/bin/dpwish
#	cd: can't cd to lib
.endif
.endif

.include <bsd.subdir_ports.mk>

# End of Makefile.
