#
# Makefile for nntpxmit
#

SRVRSRC = nntpxmit.c remote.c llist.c get_tcp_conn.c xmitauth.c ../server/fakesyslog.c ../server/strcasecmp.c

SRVROBJ = nntpxmit.o remote.o llist.o get_tcp_conn.o xmitauth.o fakesyslog.o strcasecmp.o

SRVRINC = ../common/conf.h ../common/nntp.h nntpxmit.h get_tcp_conn.h \
	llist.h sysexits.h ../common/nntp.h ../server/fakesyslog.h

SRCS	= ${SRVRSRC}

LIBS	=

CFLAGS	= -O 

# Where nntpxmit is going to live

DESTDIR	= /usr/lib/news

all:	nntpxmit shlock

nntpxmit: ${SRVROBJ} ${SRVRINC}
	${CC} ${CFLAGS} -o nntpxmit ${SRVROBJ} ${LIBS}

shlock: shlock.c
	cc ${CFLAGS} -DNNTPSRC -o shlock shlock.c

${SRVROBJ}: ${SRVRINC}

install: nntpxmit shlock
	cp nntpxmit ${DESTDIR}/nntpxmit
	cp shlock ${DESTDIR}/shlock
	chmod 755 ${DESTDIR}/nntpxmit
	chmod 755 ${DESTDIR}/shlock

lint:
	lint ${SRVRSRC}

clean:
	-rm -f *.o nntpxmit shlock make*.out a.out

distrib: clean
	rm -rf RCS save tags

check:
	ci -l *.[ch] Makefile

tags:	${SRVRSRC} ${SRVRINC}
	ctags ${SRVRSRC} ${SRVRINC}

fakesyslog.o: ../server/fakesyslog.c ../server/fakesyslog.h
	${CC} ${CFLAGS} -c ../server/fakesyslog.c

strcasecmp.o: ../server/strcasecmp.c 
	${CC} ${CFLAGS} -c ../server/strcasecmp.c

nntpxmit.o: nntpxmit.c nntpxmit.h llist.h ../common/conf.h

get_tcp_conn.o: get_tcp_conn.c get_tcp_conn.h ../common/conf.h

remote.o: remote.c nntpxmit.h get_tcp_conn.h ../common/conf.h

llist.o: llist.c llist.h

