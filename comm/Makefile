# /usr/ports/comm/Makefile Thu Feb 17 17:26:04 MEZ 1994	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

G_SUBDIR += flexfax
SUBDIR += kermit
SUBDIR += rzsz
SUBDIR += term

.if !exists(/var/spool/fax)
beforeinstall:
	mkdir /var/spool/fax	# for flexfax
.endif

.include <bsd.subdir_ports.mk>

# End of Makefile.
