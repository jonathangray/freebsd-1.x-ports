
#
#
#
SRC = tle.dat sites.dat cities.dat defaults.dat modes.dat amsat.dat

BINDIR = /usr/local/sattrack/Data

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${SRC} ${BINDIR}


.include <bsd.prog.mk>
