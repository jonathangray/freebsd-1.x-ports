# Makefile for TeX to C conversion programs.

SITEDIR=..
CC=gcc
OPT=-g
CFLAGS=$(OPT) -I$(SITEDIR) -I../common
LDFLAGS=
LOADLIBES=-lcposix
SHELL=/bin/sh

# Unix yacc works, too.
YACC=bison -y

LEX=lex
LEXLIB=-ll

all:		web2c fixwrites splitup regfix

web2c:		web2cy.o web2cl.o web2c.o
		$(CC) -o web2c $(LDFLAGS) web2c.o web2cy.o web2cl.o $(LEXLIB) \
                  $(LOADLIBES)

web2cl.o:	web2cl.c web2c.h web2cy.h symtab.h $(SITEDIR)/site.h
web2cy.o:	web2cy.c web2c.h symtab.h $(SITEDIR)/site.h
web2c.o:	web2c.c symtab.h web2cy.h $(SITEDIR)/site.h

web2cl.c:	web2c.lex $(SITEDIR)/site.h
		$(LEX) web2c.lex
		cat $(SITEDIR)/site.h lex.yy.c > web2cl.c

web2cy.c:	web2c.yacc
		$(YACC) -d web2c.yacc
		mv -f y.tab.c web2cy.c
		-if test -f web2cy.h;					\
                  then cmp y.tab.h web2cy.h || cp y.tab.h web2cy.h;	\
                  else cp y.tab.h web2cy.h;				\
                fi

fixwrites:	fixwrites.c
		$(CC) $(LDFLAGS) -o fixwrites $(CFLAGS) fixwrites.c \
                  $(LOADLIBES)

splitup:	splitup.c $(SITEDIR)/site.h
		$(CC) $(LDFLAGS) -o splitup $(CFLAGS) splitup.c $(LOADLIBES)

regfix:		regfix.c $(SITEDIR)/site.h
		$(CC) $(LDFLAGS) -o regfix $(CFLAGS) regfix.c $(LOADLIBES)

veryclean:	clean
		rm -f \#*\# *~ *.bak *.ckp core yacc.*

clean:
		rm -f *.o regfix splitup fixwrites web2c
		rm -f web2cy.c web2cl.c web2cy.h y.tab.h lex.yy.c
