#
# Makefile for nntpxfer
#
SRVRSRC = nntpxfer.c ../xmit/get_tcp_conn.c ../server/fakesyslog.c 

SRVROBJ = nntpxfer.o get_tcp_conn.o fakesyslog.o 

LIBS= -ldbm

CFLAGS= -O

nntpxfer: ${SRVROBJ}
	${CC} ${LDFLAGS} ${SRVROBJ} -o nntpxfer ${LIBS}

get_tcp_conn.o: ../xmit/get_tcp_conn.c ../xmit/get_tcp_conn.h
	${CC} ${CFLAGS} ${DEFS}	-c ../xmit/get_tcp_conn.c

fakesyslog.o: ../server/fakesyslog.c ../server/fakesyslog.h
	${CC} ${CFLAGS} ${DEFS} -c ../server/fakesyslog.c

nntpxfer.o: nntpxfer.c ../common/conf.h ../server/fakesyslog.h

clean:
	-rm -f *.o nntpxfer make*.out

distrib: clean
	rm -rf SCCS

check:;
