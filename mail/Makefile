# /usr/ports/mail/Makefile Thu Feb 17 17:26:04 MEZ 1994	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += elm

.if defined(WAIT_OK)
SUBDIR += exmh
#	cc -DBIND -DBSD42 -DBSD43 -DDBMPWD -DMORE='"/usr/bin/more"' -DSYS5DIR \
#        -DNTOHLSWAP -DOVERHEAD -DVSPRINTF -DWHATNOW -DZONEINFO -DMHE \
#        -DMHRC -DMIME -DPOSIX -DRPOP -DSENDMTS -DSMTP -DPOP \
#        -DSPRINTFTYPE=int -DTYPESIG=void -O -c vmh.c
#	vmh.c: In function `pINI':
#	vmh.c:369: structure has no member named `_maxy'
.endif

SUBDIR += mh	# vmh.c:1125: structure has no member named `_y'
SUBDIR += mush	# nate's looking at this dir. main.c:154: structure has no member named `sg_flags'
SUBDIR += popper

.include <bsd.subdir_ports.mk>

# End of Makefile.
