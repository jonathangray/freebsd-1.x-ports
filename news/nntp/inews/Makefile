#
# Makefile for pseudo-inews
#
NEWSUSR=root
SRCS	=	inews.c uname.c postauth.c ../common/clientlib.c ../common/version.c ../server/strcasecmp.c
OBJS	=	inews.o uname.o postauth.o ../common/clientlib.o ../common/version.o ../server/strcasecmp.o
HFILES	=	../common/nntp.h ../common/conf.h

CFLAGS	= -O

LIBS	=

DESTDIR	= /usr/local/bin

inews:  $(OBJS) $(HFILES)
	$(CC) $(CFLAGS) $(OBJS) -o inews $(LIBS)

uname.o: uname.c ../common/conf.h

inews.o: inews.c ../common/conf.h

../common/clientlib.o:
	${CC} ${CFLAGS} -DNNTPSRC -c ../common/clientlib.c
	mv clientlib.o ../common/clientlib.o

../common/version.o:
	${CC} ${CFLAGS} -c ../common/version.c
	mv version.o ../common/version.o

../server/strcasecmp.o:
	${CC} ${CFLAGS} -c ../server/strcasecmp.c
	mv strcasecmp.o ../server/strcasecmp.o

install: inews
	cp inews ${DESTDIR}/inews
	chown ${NEWSUSR} ${DESTDIR}/inews
	chmod 4755 ${DESTDIR}/inews

clean:
	-rm -f ${OBJS} inews make*.out

distrib: clean
	rm -f make.out
	rm -rf SCCS

check:
	ci -l *.[ch] Makefile

lint:
	lint ${SRCS} ${HFILES}

tags:
	ctags $(CFILES) $(HFILES)
