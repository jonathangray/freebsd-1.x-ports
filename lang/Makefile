# /usr/ports/lang/Makefile V2.3	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

.if !defined(AVOID)
install:
	@echo "Beware gcc1 make install blasts into where gcc2 lives !"
	exit 1
	@# exit 1 so user doesnt think other packages have installed OK
	@# nate@freefall.cdrom.com might be willing to fix this ?
SUBDIR += gcc1 #	Not in sup ports dir, but was on Gary's ports CD
#	==={> gcc1
#	==={> cc
#	"Makefile", line 8: Could not find ../../Makefile.inc
.endif

SUBDIR += perl
SUBDIR += xlispstat
SUBDIR += sather
SUBDIR += sml	# (cd src; makeml -i386 FreeBSD) cd: can't cd to src
SUBDIR += tcl
SUBDIR += tclX

.include <bsd.subdir_ports.mk>

# End of Makefile.
