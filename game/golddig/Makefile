# This program was written by Alexander Siegel in September of 1989   
# at Cornell University.  It may may copied freely for private use or 
# public dispersion provided that this comment is not removed.  This  
# program, any portion of this program, or any derivative of this     
# program may not be sold or traded for financial gain.

# Name of game executable
GAME = golddig
# Name of level editor executable
EDITOR = makelev
# Directory where games binaries are stored
BIN = /usr/X386/bin
# Directory where the library for this game will be stored
LIB = /usr/X386/lib/X11/${GAME}
# Directory where manual pages are kept
MANEXT = 6
MAN = /usr/X386/man/man${MANEXT}
# Font used to display score
SCOREFONT = -adobe-times-bold-r-normal--18-180-75-75-p-99-iso8859-1
# Height of SCOREFONT
SCORESIZE = 18

# Common compilers
#CC = cc
CC = gcc

# Some common choices for CFLAGS.
#CFLAGS = -O2
#CFLAGS = -O
CFLAGS = -O2 -W -Wreturn-type -Wunused -g -finline-functions -I/usr/X386/include
#CFLAGS = -g
LDFLAGS = -L/usr/X386/lib

all: ${GAME} ${EDITOR}

install: all
	-mkdir -p ${LIB}
	install -c -s  ${GAME} ${EDITOR} ${BIN}
# The next line sets up the level files so that anyone can edit all of
# the levels.  Remove this line to have better security.
	chmod 4755 ${BIN}/${EDITOR}
	touch ${LIB}/scores
# Allow anyone to modify the high score file.  An alternative is to
# change the mode bits of ${GAME} to be 4755.
	chmod 666 ${LIB}/scores
	cp goldlev* default ${LIB}
	-mkdir -p ${MAN}
	cp golddig.p ${MAN}/golddig.${MANEXT}

update: all
	cp ${GAME} ${EDITOR} ${BIN}
# The next line sets up the level files so that anyone can edit all of
# the levels.  Remove this line to have better security.
	chmod 4755 ${BIN}/${EDITOR}

clean:
	rm -f *.o *~ bitmap/*~ core ${GAME} ${EDITOR}

SRCS = golddig.c shared.c badguy.c movement.c scores.c
OBJS = golddig.o shared.o badguy.o movement.o scores.o

lint:
	lint -DLIB=\"${LIB}\" -DSCOREFONT=\"${SCOREFONT}\" \
                       -DSCORESIZE=${SCORESIZE} ${SRCS}

${GAME}: ${OBJS}
	${CC} ${LDFLAGS} -o ${GAME} ${OBJS} -lX11

${EDITOR}: makelev.o shared.o
	${CC} ${LDFLAGS} -o ${EDITOR} makelev.o shared.o -lX11

.c.o:
	${CC} ${CFLAGS} -c -DLIB=\"${LIB}\" -DSCOREFONT=\"${SCOREFONT}\" \
                       -DSCORESIZE=${SCORESIZE} $<

pkg: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST golddig_bin
