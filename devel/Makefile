# /usr/ports/devel/Makefile V2.3	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

tools: gmake
	cd gmake ; ${GMAKE} all install

SUBDIR =
G_SUBDIR =

SUBDIR += cflow
SUBDIR += dejagnu
SUBDIR += gdb
#	For version do grep VERSION gdb/Makefile.in
#	FreeBSD CD is 4.11 but cant understand
#	shared library executables.
#	Gary Jennejohn has (to a 4.11 base) added stuff
#	subsequently to cater for shared libraries
.if defined(WAIT_OK)
SUBDIR += gic
.endif
SUBDIR += gmake
SUBDIR += mprof30	# name told me by jkh ?
SUBDIR += mprof		# probable proper name

.include <bsd.subdir_ports.mk>

# End of Makefile.
