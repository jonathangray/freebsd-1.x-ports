#
# Makefile for slurp
#

# C compiler
CC=cc

# C compilation flags
CFLAGS= -O -I./dbz
#CFLAGS= -O -I/usr/ports/cnews/dbz

# Loader flags
LDFLAGS=

# Libraries needed
#LIBS= /usr/ports/cnews/dbz/dbz.o

# If you don't have syslog then uncomment these two lines
#FAKESRC=fakesyslog.c
#FAKEOBJ=fakesyslog.o


# Everything else probably doesn't need changing

SOURCE = slurp.c newnews.c articles.c history.c time.c sockets.c misc.c \
	 space.c ${FAKESRC}

OBJECT = slurp.o newnews.o articles.o history.o time.o sockets.o misc.o \
	 space.o ${FAKEOBJ} dbz/dbz.o

MANIFEST = README slurp.c newnews.c articles.c history.c time.c sockets.c \
	   misc.c space.c fakesyslog.c conf.h slurp.h nntp.h fakesyslog.h \
	   Makefile slurp.sys slurp.1 HISTORY

all:
	@echo "You will need to customize slurp."
	@echo "please read README.FreeBSD and follow the instructions."

install:	all


slurp: ${OBJECT}
	${CC} ${LDFLAGS} ${OBJECT} -o slurp ${LIBS}


slurp.o: slurp.c slurp.h conf.h nntp.h
newnews.o: newnews.c slurp.h conf.h nntp.h
articles.o: articles.c slurp.h conf.h nntp.h
history.o: history.c slurp.h conf.h nntp.h
time.o: time.c slurp.h conf.h
sockets.o: sockets.c slurp.h conf.h nntp.h
misc.o: misc.c slurp.h conf.h
space.o: space.c slurp.h conf.h
fakesyslog.o: fakesyslog.c slurp.h conf.h
dbz/dbz.o:
	cd dbz; make

lint:
	lint -p ${CFLAGS} ${SOURCE} > slurp.lint

clean:
	-rm -f *.o slurp

shar:
	shar -v -s -l60 -oslurp.shar ${MANIFEST}

taz:
	tar -cf - ${MANIFEST} | compress > slurp.tar.Z

diff:
	rcsdiff -q -r1.7 -C 1 ${MANIFEST} > slurp.diff

# END-OF-FILE
