# /usr/ports/lang/Makefile.inc 0.1	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

.if !defined(AVOID)
SUBDIR += gcc1 #	Not in sup ports dir, but was on Gary's ports CD
#	==={> gcc1
#	==={> cc
#	"Makefile", line 8: Could not find ../../Makefile.inc
.endif

SUBDIR += perl
# check to see if make clean fixes this:
# mv y.tab.h perly.h
# override mode 644 on perly.h? y

SUBDIR += tcl
SUBDIR += tclX7.3a

.include <bsd.subdir_ports.mk>

# End of Makefile.
