DEFINES =
COPTS = -O
CFLAGS = $(COPTS) $(DEFINES) -I../include
LINTFLAGS = $(DEFINES) -I../include -ha
LIBS= ../libcnews.a
BATCH = ../batch
THEMBIN = newsrun newsrunning c7decode bdecode recenews recpnews rnews cunbatch
THEM = newsspool $(THEMBIN)
RBIN = /bin
# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# workaround for System V make bug
SHELL = /bin/sh

all:	$(THEM)
	chmod +x $(THEM)

bininstall:	$(THEM)
	-if test ! -d $(NEWSBIN)/input ; then mkdir $(NEWSBIN)/input ; fi
	rm -f $(NEWSBIN)/input/newsspool
	cp $(THEM) $(NEWSBIN)/input
	cp rnews cunbatch $(RBIN)
	: "and newsspool needs to be made setuid-news"

cmp:	$(THEM)
	for f in $(THEM) ; do cmp $(NEWSBIN)/input/$$f $$f ; done
	cmp rnews $(RBIN)/rnews
	cmp cunbatch $(RBIN)/cunbatch
	ls -lg $(NEWSBIN)/input/newsspool | egrep -s '^-rwsrwsr-x  1 news     news'

check:	$(THEM)
	for f in $(THEM) ; do cmp $(NEWSBIN)/input/$$f $$f || true ; done
	cmp rnews $(RBIN)/rnews || true
	cmp cunbatch $(RBIN)/cunbatch || true
	ls -lg $(NEWSBIN)/input/newsspool | egrep -s '^-rwsrwsr-x  1 news     news'

newsinstall:
	: nothing

newsspool: newsspool.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) newsspool.o $(PRE) $(LIBS) $(POST) -o $@

c7decode: c7decode.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) c7decode.o $(PRE) $(LIBS) $(POST) -o $@

bdecode: bdecode.o $(BATCH)/crctab.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) bdecode.o $(BATCH)/crctab.o $(PRE) $(LIBS) $(POST) -o $@

$(BATCH)/crctab.o:	$(BATCH)/crctab.c
	( cd $(BATCH) ; make crctab.o )

bdecode.o:	bdecode.c $(BATCH)/coder.h
	$(CC) -c -I$(BATCH) $(CFLAGS) bdecode.c

cunbatch:	rnews
	cp rnews $@

lint:	newsspool.c
	lint $(LINTFLAGS) newsspool.c 2>&1 | tee lint

test.1:
	echo '#! rnews' >$@
	echo 'here is a phony first batch' >>$@

test.2:	test.2p
	: compress tends to return silly exit status for tiny inputs
	-compress -b12 <test.2p >$@

test.2p:
	echo '#! rnews' >$@
	echo 'here is a phony second batch' >>$@

test.3:	test.3c
	( echo '#! cunbatch' ; cat test.3c ) >$@

test.3c:	test.3p
	: compress tends to return silly exit status for tiny inputs
	-compress -b12 <test.3p >$@

test.3p:
	echo '#! rnews' >$@
	echo 'here is a phony third batch' >>$@

test.n1:
	: compress tends to return silly exit status for tiny inputs
	-true | compress -b12 >$@

test.n2:	test.n1
	( echo '#! cunbatch' ; cat test.n1 ) >$@

test.out:
	echo '#! rnews' >$@
	echo 'here is a phony first batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony second batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony third batch' >>$@

test.out2:
	echo '#! rnews' >$@
	echo 'here is a phony second batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony first batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony third batch' >>$@

setup:	all rtidy
	chmod +x rnews
	mkdir bin
	cp $(THEM) bin
	mkdir bin/input
	cp newsrun bin/input
	here=`pwd` ; echo "shift;shift;shift;cat \$$* >>$$here/tmp.1" >bin/relaynews
	echo "echo 10" >bin/spacefor
	echo 'ln $$*' >bin/newslock
	echo 'echo 10' >bin/sizeof
	echo ':' >bin/staleness
	echo ':' >bin/doexplode
	chmod +x bin/* bin/input/*
	mkdir in.coming

r:	all test.1 test.2 test.3 test.3c test.n1 test.n2 test.out test.out2 setup
	chmod +x $(THEM)
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./rnews <test.1
	cmp in.coming/* test.1
	expr in.coming/* : '.*\.t' >/dev/null
	sleep 2			# must delay for sake of "ls -t"!
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./rnews <test.2
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./rnews <test.n1
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./rnews <test.n2
	test `ls in.coming | wc -l` -eq 2 ;
	cmp `ls -t in.coming | sed -n '1s;^;in.coming/;p'` test.2
	expr `ls -t in.coming | sed -n '1s;^;in.coming/;p'` : '.*\.Z' >/dev/null
	sleep 2
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./rnews <test.3
	cmp `ls -t in.coming | sed -n '1s;^;in.coming/;p'` test.3c
	expr `ls -t in.coming | sed -n '1s;^;in.coming/;p'` : '.*\.Z' >/dev/null
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./newsrunning off
	test -r in.coming/stop ;
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./newsrunning on
	test ! -r in.coming/stop ;
	mkdir in.coming/bad
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./newsrun
	cmp tmp.1 test.out
	test " `echo in.coming/*`" = ' in.coming/bad' ;
	rm tmp.1
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./rnews <test.1
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./newsspool -g 4 <test.2 2>/dev/null
	echo >rnews.immed
	NEWSARTS=`pwd` NEWSCTL=`pwd` NEWSBIN=`pwd`/bin ./rnews <test.3
	cmp tmp.1 test.out2
	test " `echo in.coming/*`" = ' in.coming/bad' ;

rtidy:
	rm -f tmp.? LOCK* rnews.immed
	rm -rf bin in.coming

rclean:	rtidy
	rm -f test.*

clean:	rclean
	rm -f *.o newsspool c7decode bdecode dtr lint cunbatch
