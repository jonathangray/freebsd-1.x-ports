EXEC = call_add1
LIB = ../../libdld.a
INCLUDE = ../../dld.h
CC= gcc
CFLAGS = -I../.. -static

all:	${EXEC}

clean:
	rm -f ${EXEC} *.o

${LIB}:	../../dld.c $(INCLUDE)
	cd ../..; make ; cd test/add1

.c.o:
	${CC} -c ${CFLAGS} $*.c

add1.a: add1.o
	ar rcv add1.a add1.o

add2.a: add2.o
	ar rcv add2.a add2.o

call_add1: $(LIB) $(INCLUDE) add1.a add2.a call_add1.cc
	${CC} ${CFLAGS} call_add1.cc $(LIB) -lg++ -o call_add1
