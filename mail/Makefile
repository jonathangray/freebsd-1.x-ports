# /usr/ports/mail/Makefile.inc 0.1	Jan 94	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += elm #	Rod says things (which ?) depend on this.

.if defined(WAIT_OK)
SUBDIR += exmh
#	cc -DBIND -DBSD42 -DBSD43 -DDBMPWD -DMORE='"/usr/bin/more"' -DSYS5DIR \
#        -DNTOHLSWAP -DOVERHEAD -DVSPRINTF -DWHATNOW -DZONEINFO -DMHE \
#        -DMHRC -DMIME -DPOSIX -DRPOP -DSENDMTS -DSMTP -DPOP \
#        -DSPRINTFTYPE=int -DTYPESIG=void -O -c vmh.c
#	vmh.c: In function `pINI':
#	vmh.c:369: structure has no member named `_maxy'
.endif

SUBDIR += mh
SUBDIR += mush
#	cc -O -DCURSES -DBSD -fwritable-strings -c main.c
#	In file included from main.c:3:
#	mush.h:235: parse error before `_tty'

SUBDIR += popper

.include <bsd.subdir_ports.mk>

# End of Makefile.
