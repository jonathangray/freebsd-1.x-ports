# /usr/ports/math/Makefile Thu Feb 17 17:26:04 MEZ 1994	Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

SUBDIR =
G_SUBDIR =

SUBDIR += blas
SUBDIR += eispack
SUBDIR += linpack

G_SUBDIR += octave/octave
#	g++ -I. -I./../liboctave -I./../dld -I./.. -I. -I../liboctave -I../dld -I.. -D>
#	arith-ops.o builtins.o error.o file-io.o g-builtins.o gripes.o help.o idx-vecto>
#	../liboctave.a ../libcruft.a ../libreadline.a \
#	-ll -ltermcap -lm -lg++
#	tc-rand.cc:60: Undefined symbol _getsd referenced from text segment
#
# octave invokes /usr/local/bin/f77
# /usr/local/bin/f77 explicitly looks for /usr/bin/f2c
# -ln -s /usr/local/bin/f2c /usr/bin/f2c
# octave requires the next line
# -ln -s /usr/local/include/f2c.h /usr/include/f2c.h

.include <bsd.subdir_ports.mk>

# End of Makefile.
