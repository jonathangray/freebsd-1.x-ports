# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# =()<NEWSCTL = @<NEWSCTL>@>()=
NEWSCTL = /usr/lib/news
# workaround for System V make bug
SHELL = /bin/sh
NEWSUSER=news
NEWSGROUP=news
COPTS = -O
CFLAGS = $(COPTS) -I../include '-DNEWSUSER="$(NEWSUSER)"' '-DNEWSGROUP="$(NEWSGROUP)"'
LIBS = ../libcnews.a
LIBDIRS = libbig libbsd42 libc libcnews libfake \
	libsmall libstdio libusg libv7 libv8 dbz
DIRS = batch conf expire h hfake input $(LIBDIRS) misc relay rna explode inject
CMPDIRS = batch conf expire input man misc relay rna explode inject
FRDIRS = batch expire input relay explode inject
RDIRS = dbz $(FRDIRS)
SHS = doit.root doit.bin doit.news again.root

PGMS = spacefor queuelen

all:	$(PGMS)
	chmod +x $(PGMS)
	-if test -r hostname ; then chmod +x hostname ; fi

bininstall:	$(PGMS)
	cp $(PGMS) $(NEWSBIN)
	-if test -r hostname ; then cp hostname $(NEWSBIN) ; fi
	-if test -r dostatfs ; then cp dostatfs $(NEWSBIN) ; fi
	-if test -r doustat ; then cp doustat $(NEWSBIN) ; fi
	-if test -r doultrix ; then cp doultrix $(NEWSBIN) ; fi
	: and build takes care of setnewsids if needed

cmp:
	: nothing, but check does something

check:	$(PGMS)
	for f in $(PGMS) ; do cmp $(NEWSBIN)/$$f $$f || true ; done
	if test -r hostname ; then cmp hostname $(NEWSBIN)/hostname || true  ; fi
	if test -r dostatfs ; then cmp dostatfs $(NEWSBIN)/dostatfs || true  ; fi
	if test -r doustat ; then cmp doustat $(NEWSBIN)/doustat || true  ; fi
	if test -r doultrix ; then cmp doultrix $(NEWSBIN)/doultrix || true  ; fi

newsinstall:
	: build stuff does the real work

substs:	subst.hs subst.gc substitutions subst
	chmod +x subst
	: too late for h and hfake, include has already been built
	-egrep '^(h|hfake)/' subst.hs subst.gc >junk
	: so test to make sure they do not need it
	test ! -s junk ;
	rm -f junk
	./subst -f substitutions `sed 's;^;../;' subst.hs subst.gc`
	touch $@

sf.bsd:	sf.proto Makefile
	cp sf.proto $@

sf.v7:	sf.proto Makefile
	sed '/dfunit=/s/1024/512/;/nr = 2/s//nr = 1/;/nf = 4/s//nf = 2/;/stupid/d' sf.proto >$@

sf.sysv:	sf.proto Makefile
	sed '/dfunit=/s/1024/512/;/awk/s~|~| sed "s/.*:/: :/" |~;/nf = 4/s//nf = 3/;/stupid/s/4BSD/System V/' sf.proto >$@

sf.xenix:	sf.proto Makefile
	sed '/dfunit=/s/1024/512/;/awk/s~|~| sed "s/.*:/: :/" |~;/nr = 2/s//nr = 1/;/nf = 4/s//nf = 3/;/stupid/s/4BSD/Xenix/' sf.proto >$@

sf.sgi:	sf.proto Makefile
	sed '/dfunit=/s/1024/512/;/nf = 4/s//nf = 5/;/stupid/s/4BSD/SGI/' sf.proto >$@

sf.null:	Makefile
	echo '#! /bin/sh' >$@
	echo 'echo 100' >>$@

sf.statfs:	sf.cproto Makefile dostatfs
	cat sf.cproto >$@

sf.ultrix:	sf.cproto Makefile doultrix
	sed '/dostatfs/s//doultrix/' sf.cproto >$@

sf.ustat:	sf.cproto Makefile doustat
	sed '/dostatfs/s//doustat/' sf.cproto >$@

dostatfs:	dowhatever.o dostatfs.o
	$(CC) $(CFLAGS) $(LDFLAGS) dowhatever.o dostatfs.o $(PRE) $(LIBS) $(POST) -o $@

doultrix:	dowhatever.o doultrix.o
	$(CC) $(CFLAGS) $(LDFLAGS) dowhatever.o doultrix.o $(PRE) $(LIBS) $(POST) -o $@

doustat:	dowhatever.o doustat.o
	$(CC) $(CFLAGS) $(LDFLAGS) dowhatever.o doustat.o $(PRE) $(LIBS) $(POST) -o $@

queuelen.hdb:
	: okay

queuelen.sub:
	: okay

queuelen.old:
	: okay

queuelen.pre:	queuelen.old
	sed '/grade=d/s//grade=./' queuelen.old >$@

queuelen.null:
	echo '#! /bin/sh' >$@
	echo 'echo 0' >>$@

setnewsids:	setnewsids.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) setnewsids.o $(PRE) $(LIBS) $(POST) -o $@

../ranlibed:	../libcnews.a
	ranlib ../libcnews.a
	touch ../ranlibed

clean:
	rm -f sf.sysv sf.v7 sf.null queuelen.null sf.ustat sf.ultrix
	rm -f sf.sgi sf.xenix queuelen.pre
	rm -f *.o dostatfs doustat doultrix
	rm -f config mailname organization server whoami hostname errlog
	rm -f history history.pag history.dir active localgroups
	rm -f log mailpaths sys cron rc setnewsids setnewsids.o sf.bsd
	rm -f replyusepath spacefor queuelen junk save readnews.ctl
	rm -f postdefltdist postdefltgroup

gclean:	tidy
	rm -f ../libcnews.a ../ranlibed
	rm -rf ../include

tidy:
	for d in $(DIRS) ; do cd ../$$d ; make clean ; done

spotless:	gclean
	rm -f $(SHS)
	rm -f substs substitutions

lclean:
	rm -f ../ranlibed
	for d in $(LIBDIRS) ; do cd ../$$d ; make clean ; done

cmps:
	for d in $(CMPDIRS) ; do cd ../$$d ; make cmp RBIN=$(NEWSBIN)/input ; done

checks:
	for d in $(CMPDIRS) ; do cd ../$$d ; make check RBIN=$(NEWSBIN)/input ; done

rs:
	for d in $(RDIRS) ; do cd ../$$d ; make r ; done

frs:
	for d in $(FRDIRS) ; do cd ../$$d ; make r ; done

save:	$(SHS)
	mkdir save
	mv $(SHS) save
	cp build.def save

again:
	( echo y ; yes '' ) | build
