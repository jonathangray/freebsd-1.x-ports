# makefile for C news explode

# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# =()<NEWSCTL = @<NEWSCTL>@>()=
NEWSCTL = /usr/lib/news
# workaround for System V make bug
SHELL = /bin/sh

DEFINES=-I../include
COPTS = -O
CFLAGS= $(COPTS) $(DEFINES)
LIBS=../libcnews.a
LINT=lint
LINTFLAGS=-haz $(DEFINES)
THEM = explode doexplode

all:	explode
	chmod +x doexplode

explode: explode.o trbatch.o morefds.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ explode.o trbatch.o morefds.o $(PRE) $(LIBS) $(POST)

lint: explode.c trbatch.c
	$(LINT) $(LINTFLAGS) explode.c trbatch.c

r: explode
	(cd regress; sh regress)

newsinstall: 

# bininstall: make directories, install programs
bininstall:	install
install:	$(THEM)
	-mkdir $(NEWSBIN)/relay
	cp $(THEM) $(NEWSBIN)/relay

cmp:	$(THEM)
	for f in $(THEM) ; do cmp $(NEWSBIN)/relay/$$f $$f ; done

check:	$(THEM)
	for f in $(THEM) ; do cmp $(NEWSBIN)/relay/$$f $$f || true ; done

clean:
	rm -f *.o *.out core explode
	rm -rf regress/tmp

trbatch.o: ../include/trbatch.h
