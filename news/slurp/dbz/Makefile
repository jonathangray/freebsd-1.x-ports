COPTS = -O
CFLAGS = $(COPTS) -I../include -I.
LIBS = ../libcnews.a
DBM =
RFC = -DHAVERFCIZE
DEBUG = -DDBZDEBUG
LINTFLAGS = -h $(DEBUG) $(RFC) -I../include
LDFLAGS =
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# workaround for System V make bug
SHELL = /bin/sh

# database sizes for performance tests, regression, and regression prime-find
TSIZE=12007
RSIZE=4019
RPSIZE=2679

# history files for regression and performance tests
RHIST=hist3.3
R2HIST=hist10
THIST=hist13

it:	dbz.o

all:	dbz

bininstall:	dbz
	cp dbz $(NEWSBIN)

check:	cmp
cmp:	dbz
	cmp dbz $(NEWSBIN)/dbz

newsinstall:
	: nothing

u:	dbz.o
	ar ruv ../libcnews.a dbz.o
	cmp dbz.h ../h/dbz.h

t:	tdbz fake

lint:
	lint $(LINTFLAGS) dbzmain.c dbz.c

rdbz.o:	dbz.c
	cp dbz.c rdbz.c
	$(CC) $(CFLAGS) $(DEBUG) -DDEFSIZE=$(RSIZE) -c rdbz.c
	rm rdbz.c

rdbzmain.o:	dbzmain.c
	cp dbzmain.c rdbzmain.c
	$(CC) $(CFLAGS) $(RFC) -c rdbzmain.c
	rm rdbzmain.c

tdbz.o:	dbz.c
	cp dbz.c tdbz.c
	$(CC) $(CFLAGS) -DDEFSIZE=$(TSIZE) -c tdbz.c
	rm tdbz.c

dbz:	dbzmain.o
	$(CC) $(CFLAGS) $(LDFLAGS) dbzmain.o $(PRE) $(DBM) $(LIBS) $(POST) -o $@

tdbz:	dbzmain.o tdbz.o
	$(CC) $(CFLAGS) $(LDFLAGS) dbzmain.o tdbz.o $(PRE) $(LIBS) $(POST) -o $@

rdbz:	rdbzmain.o rdbz.o
	$(CC) $(CFLAGS) $(LDFLAGS) rdbzmain.o rdbz.o $(PRE) $(LIBS) $(POST) -o $@

fake:	fake.o random.o
	$(CC) $(CFLAGS) $(LDFLAGS) fake.o random.o $(PRE) $(LIBS) $(POST) -o $@

byteflip:	byteflip.o
	$(CC) $(CFLAGS) $(LDFLAGS) byteflip.o $(PRE) $(LIBS) $(POST) -o $@

hist10:	fake
	./fake -t -e 75 10000 >$@

hist3.3:	fake
	./fake -t -e 75 3300 >$@

hist13:	fake
	./fake -t -e 75 13000 >$@

r:	rdbz $(RHIST) $(R2HIST) byteflip getmap revbytes altbytes
	: 'WARNING:  creates about 2MB of debris; do "make rclean" afterward'
	rm -f dbase dbase[23] dbase.* dbase[23].*
	test ! -d xx || rmdir xx
	: crude check of synthetic history file
	( sed 25q $(RHIST) ; tail -25 $(RHIST) ) >histjunk
	cmp histjunk firstlast25
	rm histjunk
	: basic tests, exercising as many options as possible
	cp $(RHIST) dbase
	mkdir xx
	chmod -w xx
	./rdbz -E 1000 -0 -M -i -S -u -U -C xx dbase
	rmdir xx
	sed '/>	0/d' $(RHIST) >dbase.used
	test "`cat dbase.used | wc -l`" -eq "`sed -n '2s/ .*//p' dbase.dir`" ;
	cp $(RHIST) dbase2
	./rdbz -E 1000 -0 -p $(RPSIZE) -t '	' dbase2
	cmp $(RHIST) dbase
	cmp dbase dbase2
	cmp dbase.dir dbase2.dir
	cmp dbase.pag dbase2.pag
	./rdbz -E 1000 -0 -c dbase
	./rdbz -E 1000 -0 -c -i -q -M -U dbase
	: build a database and then add to it
	sed 1000q $(RHIST) >dbase2
	sed 1,1000d $(RHIST) >dbase2.add
	./rdbz -E 1000 -0 dbase2
	./rdbz -E 1000 -0 -a dbase2 dbase2.add
	cmp dbase dbase2
	cmp dbase.dir dbase2.dir
	cmp dbase.pag dbase2.pag
	: build based on existing one, test extraction and readonly files
	./rdbz -E 1000 -0 -f dbase dbase2
	test "`cat dbase.used | wc -l`" -eq "`awk 'NR==2{print $$1}' dbase2.dir`" ;
	test "`cat dbase.used | wc -l`" -eq "`awk 'NR==2{print $$2}' dbase2.dir`" ;
	chmod -w dbase2.dir dbase2.pag
	./rdbz -E 1000 -x dbase2 dbase >dbase.temp
	cmp dbase.used dbase.temp
	: try some small case perversions
	sed 's/\(@[^ 	]*\)A/\1a/' dbase >dbase.ick
	./rdbz -E 1000 -x dbase2 dbase.ick >dbase.temp
	cmp dbase.used dbase.temp
	sed -n 's/A\([^ 	]*@\)/a\1/p' dbase >dbase.ick
	./rdbz -x dbase2 dbase.ick >dbase.temp
	test ! -s dbase.temp ;
	rm -f dbase2.dir dbase2.pag
	: try it without tags, case-insensitive, with case perversions
	./rdbz -E 1000 -0 -p '0 b 1' dbase2
	tr '[A-M][n-z]' '[a-m][N-Z]' <dbase2 >dbase.ick
	./rdbz -E 1000 -x dbase2 dbase.ick >dbase.temp
	cmp dbase.used dbase.temp
	rm -f dbase.temp dbase.ick
	: test various perversions of byte ordering
	awk -f revbytes dbase.dir >dbase2.dir
	chmod +x getmap
	./byteflip `./getmap dbase.dir` `./getmap dbase2.dir` <dbase.pag >dbase2.pag
	cp dbase dbase2
	./rdbz -E 1000 -0 -c dbase2
	awk -f altbytes dbase.dir >dbase2.dir
	dd conv=swab <dbase.pag >dbase2.pag
	./rdbz -E 1000 -0 -c dbase2
	cp dbase2 dbase3
	./rdbz -E 1000 -0 -f dbase2 dbase3
	./rdbz -E 1000 -0 -c dbase3
	test " `./getmap dbase2.dir`" = " `./getmap dbase3.dir`" ;
	: test massive overflow, throw in case sensitivity and tag mask
	cp $(R2HIST) dbase
	./rdbz -E 1000 -0 -p '0 0 7ffc0000' dbase
	./rdbz -E 1000 -0 -cq dbase
	sed 100q dbase | egrep '[aA].*	' | tr aA Aa >dbase.ick
	./rdbz -x dbase dbase.ick >dbase.temp
	test ! -s dbase.temp ;
	: success!

rclean:
	rm -f dbase dbase[23] dbase.* dbase[23].* fake fake.o random.o
	rm -f rdbz rdbz.o rdbzmain.o $(RHIST) $(R2HIST) byteflip byteflip.o
	rm -f histjunk core
	test ! -d xx || rmdir xx

clean:	rclean
	rm -f *.o dbz [a-z]dbz [a-z][a-z]dbz junk* PostScript.out
	rm -f hist* dbase* *.bak mon.out gmon.out core dbm.h
