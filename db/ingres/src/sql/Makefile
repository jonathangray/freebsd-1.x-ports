# Generated automatically from Makefile.in by configure.
# %W% %G%

# make and run the sql example programs for
# lex & yacc, Second Edition
#
CC = gcc
DEFS=  -DHAVE_STRING_H=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 -DHAVE_SGTTY_H=1 -DHAVE_FCNTL_H=1 -DHAVE_MACHINE_ENDIAN_H=1 -DHAVE_SYS_WAIT_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_RESOURCE_H=1 -DTIME_WITH_SYS_TIME=1 -DRETSIGTYPE=void -DDIRENT=1 -DDECLARED_SYS_NERR=1 -DDECLARED_SYS_ERRLIST=1 -DHAVE_STRERROR=1 -DHAVE_STRCSPN=1
RANLIB= ranlib
OPTIM= -O
MOAN= -Wall
LDFLAGS= $(OPTIM)
CFLAGS= -DYYDEBUG=1 $(OPTIM) $(DEFS) $(MOAN)
LIBS = -ly -ll
LEX = flex
YACC = yacc

PROGRAMS5 = sql1 sql2

all:	${PROGRAMS5} 

sql1:	sql1.o scn1.o
	${CC} -o $@ sql1.o scn1.o ${LIBS}

sql1.c sql1.h:	sql1.y
	${YACC} -d sql1.y
	mv y.tab.h sql1.h
	mv y.tab.c sql1.c

scn1.o:	sql1.h scn1.c

sql2:	sql2.o scn2.o sqltext.o
	${CC} -o $@ sql2.o scn2.o sqltext.o ${LIBS}

sql2.c sql2.h:	sql2.y
	${YACC} -d sql2.y
	mv y.tab.h sql2.h
	mv y.tab.c sql2.c

scn2.o:	sql2.h scn2.c

clean:
	rm -f *.o sql1.h sql1.c sql2.h sql2.c scn1.c scn2.c a.out *core
