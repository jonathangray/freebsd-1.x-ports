# Configuration stuff.
COPTS=-O
CFLAGS=$(COPTS) -I../include
LIBS=../libcnews.a
# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# =()<NEWSCTL = @<NEWSCTL>@>()=
NEWSCTL = /usr/lib/news
# workaround for System V make bug
SHELL = /bin/sh

OUTGOING = $(NEWSARTS)/out.going
PGMDIR = $(NEWSBIN)/batch

PGMS=batcher batchih batchsm batchsplit comp compcun nocomp viainews viauux \
	sendbatches compc7 c7encode viamail viapmail bencode compb viauuxz \
	viaemail viarsh viauuxl viauuxcun batchcheck batchra vianowhere \
	comp12
BP = batchparms.eg
ALL = $(PGMS) $(BP)

all:	$(ALL)
	chmod +x $(PGMS)

batcher:	batcher.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) batcher.o $(PRE) $(LIBS) $(POST) -o $@

c7encode:	c7encode.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) c7encode.o $(PRE) $(LIBS) $(POST) -o $@

bencode:	bencode.o crctab.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) bencode.o crctab.o $(PRE) $(LIBS) $(POST) -o $@

bencode.o:	coder.h

bininstall:	$(ALL)
	-if test ! -d $(PGMDIR) ; then mkdir $(PGMDIR) ; fi
	cp $(PGMS) $(PGMDIR)

cmp:	$(PGMS)
	for f in $(PGMS) ; do cmp $(PGMDIR)/$$f $$f ; done

check:	$(PGMS)
	for f in $(PGMS) ; do cmp $(PGMDIR)/$$f $$f || true ; done

newsinstall:	$(BP)
	-if test ! -d $(OUTGOING) ; then mkdir $(OUTGOING) ; fi
	-if test ! -r $(NEWSCTL)/$(BP) ; then cp $(BP) $(NEWSCTL) ; fi

viauuxz:	viauux
	sed '$$s/-r/-r -z/' viauux >$@

viauuxcun:	viauux
	sed '$$s/rnews/cunbatch/' viauux >$@

viaemail:	viamail
	sed '$$s/rnews/enews/' viamail >$@

comp12:	comp
	sed '/^compress$$/s//& -b 12/' comp >$@

test.1:
	echo 'a test' >$@

test.2:
	echo 'another test' >$@

test.3:
	cat batcher.c batcher.c batcher.c batcher.c batcher.c >$@

r:	all test.1 test.2 test.3
	echo x 1 >togo
	echo a 1 >>togo
	echo b 1 >>togo
	echo c 1 >>togo
	echo d 1 >>togo
	echo e 1 >>togo
	echo f 1 >>togo
	echo g 1 >>togo
	echo h 1 >>togo
	echo i 3 >>togo
	echo j 20 >>togo
	echo k 99999 >>togo
	for f in a b c d e f g h i j k ; do >$$f ; done	# but not x
	rm -rf bin togo.*
	mkdir bin
	echo 'ln $$* >/dev/null 2>/dev/null' >bin/newslock
	echo 'cat' >bin/munch
	echo "( echo --- ; cat ) >>`pwd`/output" >bin/xmit
	echo 'echo 2 ; cat' >bin/munch2
	echo "( echo +++ ; cat ) >>`pwd`/output2" >bin/xmit2
	echo 'echo 4' >bin/queuelen
	echo 'echo 99' >bin/spacefor
	ln batcher bin/batcher
	ln batchsplit bin/batchsplit
	ln batchcheck bin/batchcheck
	chmod +x bin/*
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./batchsplit -x 16
	test ! -s togo ;
	test " `wc -l <togo.next`" -eq 11 ;	# x through j
	test ! -f togo.1 ;
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./batchsplit -x 16
	test " `cat togo.1`" = " a 1" ;
	test " `cat togo.2`" = " b 1" ;
	test " `cat togo.3`" = " c 1" ;
	test " `cat togo.4`" = " d 1" ;
	test " `cat togo.5`" = " e 1" ;
	test " `cat togo.6`" = " f 1" ;
	test " `cat togo.7`" = " g 1" ;
	echo h 1 >test.good ; echo i 3 >>test.good ; echo j 20 >>test.good
	cmp test.good togo.next
	echo k 99999 >test.good ; cmp test.good togo.more
	test ! -s togo ;
	rm togo.* [a-k]
	echo `pwd`/test.1 >togo
	echo ./test.2 >>togo
	echo `pwd`/test.3 666 >>togo
	./batcher togo >test.out
	echo "#! rnews `wc -c test.1 | awk '{print $$1}'`" >test.good
	cat test.1 >>test.good
	echo "#! rnews `wc -c test.2 | awk '{print $$1}'`" >>test.good
	cat test.2 >>test.good
	( echo --- ; cat test.good ) >test.out1
	( echo +++ ; echo 2 ; cat test.good ) >test.out2
	echo "#! rnews `wc -c test.3 | awk '{print $$1}'`" >test.temp
	cat test.3 >>test.temp
	cat test.temp >>test.good
	( echo --- ; cat test.temp ) >>test.out1
	cmp test.good test.out
	rm test.out togo
	rm -rf out.going batchparms test.out test.good output output2
	mkdir out.going out.going/foo out.going/bar
	echo 'foo	500	10	batcher	munch	xmit' >batchparms
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./sendbatches -d >test.out
	echo foo >test.good
	cmp test.out test.good
	echo '/default/	10000	5	batcher	munch2	xmit2' >>batchparms
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./sendbatches -d | sort >test.out
	( echo foo ; echo bar ) | sort >test.good
	cmp test.out test.good
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./sendbatches -d bletch >test.out
	echo bletch >test.good
	cmp test.out test.good
	>batchlog
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./sendbatches
	test ! -s output ;
	test ! -s output2 ;
	echo `pwd`/test.1 200 >out.going/foo/togo
	echo `pwd`/test.2 200 >>out.going/foo/togo
	echo `pwd`/test.3 200 >>out.going/foo/togo
	echo `pwd`/test.1 4000 >out.going/bar/togo
	echo `pwd`/test.2 4000 >>out.going/bar/togo
	echo `pwd`/test.3 4000 >>out.going/bar/togo
	echo `pwd`/test.3 4000 >test.left
	NEWSARTS=`pwd` NEWSCTL=`pwd` ./sendbatches
	cmp output test.out1
	cmp output2 test.out2
	test ! -s out.going/foo/togo ;
	test ! -s out.going/bar/togo ;
	cmp test.left out.going/bar/togo.next
	sed '/^$$/q' batchlog | sed '1d;$$d' | sed '$$d' >test.log
	test ! -s test.log ;
	sed '1,/^$$/d' batchlog | sed '1d;$$d' | sed '$$d' >test.log
	echo 'bar	backlog 1 (batches flowing)' >test.good
	cmp test.good test.log
	rm -rf test.* togo out.going bin batchparms batchlog batchlog.*
	rm -f output output2

clean:
	rm -rf out.going bin
	rm -f *.o test.* togo togo.* batchparms batcher viauuxcun
	rm -f batchlog batchlog.* c7encode bencode viauuxz viaemail comp12
