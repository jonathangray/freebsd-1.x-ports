DEFS = # -Dvoid=int
COPTS = -O
CFLAGS = $(DEFS) $(COPTS) -I../include
LIBS = ../libcnews.a
DBM =
LINTFLAGS = $(DEFS) -ha
RN = ../relay
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# workaround for System V make bug
SHELL = /bin/sh

MAINTBIN= locknews
MAINT = $(MAINTBIN) newsdaily newswatch newsboot addgroup delgroup \
	adddirs addfeed act.to.times histfrom newsfrom addmissing newshist
UTILBIN = gngp newslock ctime getdate canonhdr getabsdate sizeof
UTILS = $(UTILBIN) newshostname
THEM = $(MAINT) $(UTILS)

all:	$(THEM)
	chmod +x $(THEM)

bins:	$(UTILBIN)

bininstall: $(THEM)
	-if test ! -d $(NEWSBIN)/maint ; then mkdir $(NEWSBIN)/maint ; fi
	cp $(MAINT) $(NEWSBIN)/maint
	cp $(UTILS) $(NEWSBIN)

cmp:	$(THEM)
	for f in $(MAINT) ; do cmp $(NEWSBIN)/maint/$$f $$f ; done
	for f in $(UTILS) ; do cmp $(NEWSBIN)/$$f $$f ; done

check:	$(THEM)
	for f in $(MAINT) ; do cmp $(NEWSBIN)/maint/$$f $$f || true ; done
	for f in $(UTILS) ; do cmp $(NEWSBIN)/$$f $$f || true ; done

newsinstall:
	: nothing

gngp:	gngp.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) gngp.o $(PRE) $(LIBS) $(POST) -o $@

ctime:	ctime.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) ctime.o $(PRE) $(LIBS) $(POST) -o $@

getdate:	getdate.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) getdate.o $(PRE) $(LIBS) $(POST) -o $@

newslock:	newslock.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) newslock.o $(PRE) $(LIBS) $(POST) -o $@

locknews:	locknews.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) locknews.o $(PRE) $(LIBS) $(POST) -o $@

canonhdr:	canonhdr.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) canonhdr.o $(PRE) $(LIBS) $(POST) -o $@

getabsdate:	getabsdate.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) getabsdate.o $(PRE) $(LIBS) $(POST) -o $@

sizeof:	sizeof.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) sizeof.o $(PRE) $(LIBS) $(POST) -o $@

clean:
	rm -f *.o $(MAINTBIN) $(UTILBIN)
