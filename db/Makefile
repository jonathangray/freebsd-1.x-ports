# /usr/ports/db/Makefile.inc 0.1	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

.if !defined(AVOID)
SUBDIR += ingres
#	Before compiling, ingres must have a login,
#	the source must be in ingres's login directory,
#	and compiling must be done by user ingres
#	using GCC version 1.x
#	See the README files for more information.
.endif

.include <bsd.subdir_ports.mk>

# End of Makefile.
