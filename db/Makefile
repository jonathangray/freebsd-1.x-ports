# /usr/ports/db/Makefile Thu Feb 17 17:26:04 MEZ 1994	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

.if !defined(AVOID)
SUBDIR += ingres
#	Before compiling, ingres must have a login,
#	the source must be in ingres's login directory,
#	and compiling must be done by user ingres using GCC version 1.x
#	See the ingres/README files for more information.
.endif

.include <bsd.subdir_ports.mk>

# End of Makefile.
