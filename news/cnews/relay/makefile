# makefile for C news relaynews

# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# =()<NEWSCTL = @<NEWSCTL>@>()=
NEWSCTL = /usr/lib/news
# workaround for System V make bug
SHELL = /bin/sh

I=../include
BIN=/bin
NPROC=2
# -DVOID=int for libc.h & old lint libraries
DEFINES= -I$(I) -I. -DVOID=int -DFLUSHEVERY=6
#CC=agccped
#CC=CC +V
COPTS= -O # -pg -g -O
CFLAGS=$(DEFINES) $(COPTS)
DBM = # -ldbm
#LIBS= $(DBM) /usr/lib/debug/malloc.o
LIBS= $(DBM)
LINT=lint
LINTFLAGS=-haz $(DEFINES)
LLIBS= # -lmalloc
# I wish I could make lint shut the fk up about some things.  Grrr!
LINTFILT=egrep -v '(possible pointer|long assign|nnfree|getdate|:$$)'
PROPTS=
P=stpr
PP=pp -Tpsc -fR # lazywriter
PPBACK=dps | stps # lazywriter

LIBOBJS=../libcnews.a
SRC=relaynews.c active.c article.c caches.c mkdirs.c control.c fileart.c \
	hdrdefs.c hdrparse.c hdrmunge.c \
	history.c io.c msgs.c procart.c \
	sys.c transmit.c trbatch.c ihave.c $(LIBSRCS) \
	morefds.c
OBJ=relaynews.o active.o article.o caches.o mkdirs.o control.o fileart.o \
	hdrdefs.o hdrparse.o hdrmunge.o \
	history.o io.o msgs.o procart.o \
	sys.o transmit.o trbatch.o ihave.o \
	morefds.o
FILES=$(NONCFILES) $(CFILES)
NONCFILES= TODO* README ads/README ads/[0-9]* makefile
CFILES= $(I)/*.h \
	active.h article.h batchnames.h caches.h mkdirs.h control.h fileart.h \
	hdrint.h headers.h history.h system.h transmit.h $(SRC)

all: makefile relaynews
	chmod +x aux/[a-z]* ctl/[a-z]*

mkfile: makefile
	sed '/mkfile/d' makefile | mkconv | sed 's/make/mk/g' >$@

relaynews: $(OBJ) $(LIBOBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) $(OBJ) $(PRE) $(LIBS) $(LIBOBJS) $(POST) -o $@
lint: $(SRC)
	$(LINT) $(LINTFLAGS) $(SRC) $(LLIBS) 2>&1 | $(LINTFILT)
lint-p: $(SRC)
	$(LINT) $(LINTFLAGS) -p $(SRC) $(LLIBS) 2>&1 | $(LINTFILT)

newsinstall:

# bininstall: make directories, install programs
bininstall: install
install: $(NEWSBIN)/relay/relaynews
$(NEWSBIN)/relay/relaynews: relaynews
	-mkdir $(NEWSBIN)/relay $(NEWSBIN)/ctl
	cp ctl/[a-z]* $(NEWSBIN)/ctl
	cp aux/[a-z]* $(NEWSBIN)/relay
	rm -f $(NEWSBIN)/relay/relaynews
	cp relaynews $(NEWSBIN)/relay
	: needs to be news-owned, setuid -- build looks after that

cmp:	relaynews
	cmp $(NEWSBIN)/relay/relaynews relaynews
	for f in `../xls ctl | egrep -v alternate` ; do cmp $(NEWSBIN)/ctl/$$f ctl/$$f ; done
	for f in `../xls aux` ; do cmp $(NEWSBIN)/relay/$$f aux/$$f ; done
	ls -lg $(NEWSBIN)/relay/relaynews | egrep -s '^-rwsrwsr-x  1 news     news'

check:	relaynews
	cmp $(NEWSBIN)/relay/relaynews relaynews || true
	for f in `../xls ctl | egrep -v alternate` ; do cmp $(NEWSBIN)/ctl/$$f ctl/$$f || true ; done
	for f in `../xls aux` ; do cmp $(NEWSBIN)/relay/$$f aux/$$f || true ; done
	ls -lg $(NEWSBIN)/relay/relaynews | egrep -s '^-rwsrwsr-x  1 news     news'

TODO.grep: TODO
	-egrep TODO $(I)/*.h *.h *.c | tr -s " \11" " " >$@
	-egrep TODO ../lib*/*.[ch] | tr -s " \11" " " >>$@

print: printc printnonc
	touch $@
printc: $(CFILES)
	$(PP) $? | $(PPBACK)
	touch $@
printnonc: $(NONCFILES)
	pr $(PROPTS) $? | $P
	touch $@
clean:
	rm -rf core a.out relaynews *.o	regress/tmp

r:	relaynews
	chmod +x regress/regress
	(cd regress; sh regress)
	
# header dependencies follow
active.o: $(I)/libc.h $(I)/news.h $(I)/config.h active.h
article.o: $(I)/libc.h $(I)/news.h headers.h article.h
caches.o: $(I)/news.h active.h caches.h transmit.h
control.o: $(I)/libc.h $(I)/news.h $(I)/case.h $(I)/config.h control.h headers.h
control.o: article.h caches.h history.h
fileart.o: $(I)/fixerrno.h $(I)/libc.h $(I)/news.h $(I)/config.h control.h active.h
fileart.o: fileart.h mkdirs.h headers.h article.h history.h $(I)/ngmatch.h
fileart.o: system.h
hdrdefs.o: $(I)/libc.h $(I)/news.h headers.h hdrint.h
hdrmunge.o: $(I)/libc.h $(I)/news.h $(I)/case.h fileart.h headers.h article.h
hdrmunge.o: hdrint.h $(I)/msgs.h
hdrparse.o: $(I)/libc.h $(I)/news.h control.h $(I)/case.h fileart.h headers.h
hdrparse.o: hdrint.h
history.o: $(I)/fixerrno.h $(I)/libc.h $(I)/news.h $(I)/config.h $(I)/dbz.h $(I)/fgetmfs.h
history.o: headers.h article.h history.h $(I)/msgs.h
ihave.o: $(I)/libc.h $(I)/news.h $(I)/config.h batchnames.h headers.h article.h
ihave.o: history.h $(I)/fgetmfs.h $(I)/msgs.h transmit.h
io.o: $(I)/news.h headers.h article.h $(I)/msgs.h
mkdirs.o: $(I)/libc.h $(I)/news.h
msgs.o: $(I)/news.h headers.h article.h $(I)/msgs.h rmsgs.h
procart.o: $(I)/libc.h $(I)/news.h active.h control.h headers.h article.h
procart.o: history.h io.h $(I)/msgs.h $(I)/ngmatch.h system.h transmit.h
relaynews.o: $(I)/fixerrno.h $(I)/libc.h $(I)/news.h $(I)/config.h $(I)/fgetmfs.h
relaynews.o: active.h caches.h fileart.h headers.h history.h transmit.h
sys.o: $(I)/fixerrno.h $(I)/libc.h $(I)/fgetmfs.h $(I)/news.h $(I)/config.h batchnames.h
sys.o: $(I)/ngmatch.h system.h
transmit.o: $(I)/libc.h $(I)/news.h $(I)/config.h headers.h active.h article.h
transmit.o: $(I)/msgs.h $(I)/ngmatch.h system.h $(I)/trbatch.h transmit.h
trbatch.o: $(I)/hdbm.h $(I)/hash.h $(I)/libc.h $(I)/news.h $(I)/config.h $(I)/msgs.h
trbatch.o: $(I)/trbatch.h
