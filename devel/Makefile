# /usr/ports/devel/Makefile Thu Feb 17 17:26:04 MEZ 1994	by Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

tools: gmake
	cd gmake ; ${GMAKE} all install

SUBDIR =
G_SUBDIR =

SUBDIR += cflow
SUBDIR += dejagnu
SUBDIR += gdb
.if defined(WAIT_OK)
SUBDIR += gic
.endif
SUBDIR += gmake
SUBDIR += mprof

.include <bsd.subdir_ports.mk>

# End of Makefile.
