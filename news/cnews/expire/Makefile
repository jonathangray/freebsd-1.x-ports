PROF = 
COPTS = -O
CFLAGS = $(COPTS) -I../include $(PROF)
LINTFLAGS = -I../include
JUNKLINT = 'possible pointer align'
DBM =
LIBS= ../libcnews.a
THEM = expire histdups histinfo histslash mkhistory \
	upact doexpire mkadir recovact expireiflow
DTR = README Makefile dircheck doexpire expire.c histdups histinfo.c \
	histslash.c mkhistory pgood tgood upact \
	mkadir updatemin.c recovact
UPACT=upact
# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# =()<NEWSCTL = @<NEWSCTL>@>()=
NEWSCTL = /usr/lib/news
# workaround for System V make bug
SHELL = /bin/sh

all:	$(THEM)
	chmod +x $(THEM)

bininstall:	$(THEM)
	-if test ! -d $(NEWSBIN)/expire ; then mkdir $(NEWSBIN)/expire; fi
	cp $(THEM) $(NEWSBIN)/expire

explists:	explist.no explist.yes

cmp:	$(THEM)
	for f in $(THEM) ; do cmp $(NEWSBIN)/expire/$$f $$f ; done

check:	$(THEM)
	for f in $(THEM) ; do cmp $(NEWSBIN)/expire/$$f $$f || true ; done

newsinstall:	explist
	-if test ! -r $(NEWSCTL)/explist ; then cp explist $(NEWSCTL)/explist ; fi

expire: expire.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) expire.o $(PRE) $(DBM) $(LIBS) $(POST) -o $@

histinfo: histinfo.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) histinfo.o $(PRE) $(LIBS) $(POST) -o $@

updatemin:	updatemin.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) updatemin.o $(PRE) $(LIBS) $(POST) -o $@

histslash:	histslash.o $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) histslash.o $(PRE) $(LIBS) $(POST) -o $@

lint:	expire.c
	lint -ha $(LINTFLAGS) expire.c 2>&1 | egrep -v $(JUNKLINT) | tee $@

explist.yes:
	echo "# note that the order of lines in this file is significant" >$@
	echo >>$@
	echo "# hold onto history lines 14 days, nobody gets >90 days" >>$@
	echo "/expired/			x	14	-" >>$@
	echo "/bounds/			x	0-1-90	-" >>$@
	echo >>$@
	echo "# override later defaults for some groups of note" >>$@
	echo "sci.space.shuttle,rec.birds	x	7	@" >>$@
	echo >>$@
	echo "# big non-tech groups held long enough for a long weekend" >>$@
	echo "sci,rec,talk,soc,misc,alt	u	4	-" >>$@
	echo >>$@
	echo "# real noise gets thrown away fast" >>$@
	echo "news.groups			x	2	-" >>$@
	echo "junk,tor.news.stats		x	2	-" >>$@
	echo >>$@
	echo "# throw away some technical stuff not worth archiving" >>$@
	echo "comp.os.vms,comp.mail.maps	x	7	-" >>$@
	echo >>$@
	echo "# default:  7 days and archive" >>$@
	echo "all				x	7	@" >>$@

explist.no:	explist.yes
	echo '# modified version, no archiving $(COPTS)' >$@
	echo '#' >>$@
	sed 's/@/-/' explist.yes >>$@

explist:	explist.no
	cp explist.no $@

# setup for regression test
setup:	$(THEM) ../dbz/dbz
	rm -f explist history active active.after history.proto
	rm -rf arts arch arch2 arch3 nbin ctl
	>history.pag
	>history.dir
	echo "/expired/	x	0.08	-" >>explist
	echo '/bounds/	x	0.01-0.1-0.11	- ' >>explist
	echo '#comment' >>explist
	echo >>explist
	echo ' ' >>explist
	echo "bar,!bar.ugh	x	0.05	`pwd`/arch2" >>explist
	echo 'mod	m	0.1	@' >>explist
	echo 'mod	u	0.05	-' >>explist
	echo 'urp	x	0.01-0.05-0.1	-' >>explist
	echo "bletch	x	0.1	=`pwd`/arch3/bletch" >>explist
	echo 'all	x	0.1	@' >>explist
	mkdir nbin nbin/expire ctl
	for f in $(THEM) ; do ln $$f nbin/expire/$$f ; done
	echo 'ln $$* >/dev/null 2>/dev/null' >nbin/newslock
	echo 'echo 10' >nbin/spacefor
	ln ../dbz/dbz nbin/dbz
	chmod +x nbin/* nbin/expire/*
	mkdir arts arts/foo arts/bar arts/bar/ugh arts/urp arch arch2
	mkdir arts/mod arts/mod/mod arts/mod/unmod arch3 arch3/bletch
	mkdir arts/bletch arts/lost+found
	echo 'foo 00099 00000 y ' >>active
	echo 'foo 00099 00001 y' >>active.after
	echo 'bar 00099 00000 m' >>active
	echo 'bar 00099 00100 m' >>active.after
	echo 'bar.ugh 00099 00000 m' >>active
	echo 'bar.ugh 00099 00099 m' >>active.after
	echo 'urp 00099 00000 n' >>active
	echo 'urp 00099 00007 n' >>active.after
	echo 'mod.mod 00013 00000 m' >>active
	echo 'mod.mod 00013 00013 m' >>active.after
	echo 'mod.unmod 00016 00000 y' >>active
	echo 'mod.unmod 00016 00016 y' >>active.after
	echo 'bletch 00099 00000 y' >>active
	echo 'bletch 00099 00100 y' >>active.after
	echo nevermore >arts/lost+found/1
	echo '<wont1>	9999~-	foo/1' >>history.proto
	echo :foo/1: >arts/foo/1
	echo 'MeSsAge-ID: <wont1>' >>arts/foo/1
	echo '<will2>	1000~-	foo/2' >>history.proto
	echo foo/2 >arts/foo/2
	echo '<will3>	1000~100	foo/3' >>history.proto
	echo foo/3 >arts/foo/3
	echo '<two4>	100	bar/4' >>history.proto
	echo :bar/4: >arts/bar/4
	echo 'SuBjeCt: yes' >>arts/bar/4
	echo '<will5>	100~	bar.ugh/5' >>history.proto
	echo :bar/ugh/5: >arts/bar/ugh/5
	echo >>arts/bar/ugh/5
	echo 'Subject: no' >>arts/bar/ugh/5
	echo '<gone6>	100~-	urp/6' >>history.proto
	echo urp/6 >arts/urp/6
	echo '<wont7>	9999~9999~xx~a	urp/7' >>history.proto
	echo :urp/7: >arts/urp/7
	echo 'mEssaGe-id: <wont7>' >>arts/urp/7
	echo 'exPiRes: 9999' >>arts/urp/7
	echo '<gone8>	100~20000	urp/8' >>history.proto
	echo urp/8 >arts/urp/8
	echo '<gone9>	100~-	urp/9' >>history.proto
	echo '<wont10>	1000~20000	foo/10' >>history.proto
	echo :foo/10: >arts/foo/10
	echo 'Message-ID: <wont10>' >>arts/foo/10
	echo 'Expires:  20000' >>arts/foo/10
	echo 'Supersedes: <wont1>' >>arts/foo/10
	echo >>arts/foo/10
	echo '<will11>	100~20000	foo/11' >>history.proto
	echo foo/11 >arts/foo/11
	echo '<will12>	100~-	mod.mod/12' >>history.proto
	echo mod/mod/12 >arts/mod/mod/12
	echo '<wont13>	2000	mod.mod/13' >>history.proto
	echo :mod/mod/13: >arts/mod/mod/13
	echo 'Message-ID: <wont13>' >>arts/mod/mod/13
	echo '<gone14>	100~-	mod.unmod/14' >>history.proto
	echo mod/unmod/14 >arts/mod/unmod/14
	echo '<gone15>	2000	mod.unmod/15' >>history.proto
	echo mod/unmod/15 >arts/mod/unmod/15
	echo '<wont16>	9999~-	mod.unmod/16' >>history.proto
	echo :mod/unmod/16: >arts/mod/unmod/16
	echo 'Message-ID:	<wont16>' >>arts/mod/unmod/16
	echo '<three17>	100~-	bletch/17' >>history.proto
	echo bletch/17 >arts/bletch/17
	egrep wont history.proto >history.after
	echo '<gone96>	5000~-	/' >>history.proto
	echo '<gone96>	5000~-' >>history.after
	echo '<gone97>	5000~-' >>history.proto
	echo '<gone97>	5000~-' >>history.after
	echo '<gone98>	5000~-	urp/98' >>history.proto
	echo '<gone98>	5000~-' >>history.after
	echo urp/98 >arts/urp/98
	echo '<multi99>	5000~-	bar/99, bar.ugh/99  foo/99 urp/99' >>history.proto
	echo '<multi99>	5000~-	bar.ugh/99 foo/99' >>history.after
	echo :foo/99:bar/99:bar/ugh/99:urp/99: >arts/foo/99
	echo 'Message-ID:  <multi99>' >>arts/foo/99
	ln arts/foo/99 arts/bar/99
	ln arts/foo/99 arts/bar/ugh/99
	ln arts/foo/99 arts/urp/99
	cp history.proto history

# regression test machinery
D = NEWSARTS=`pwd`/arts NEWSCTL=`pwd` NEWSBIN=`pwd`/nbin
DH = NEWSARTS=`pwd`/arts NEWSCTL=`pwd`/ctl NEWSBIN=`pwd`/nbin
RUN = $(DH) ./expire -H `pwd` -a `pwd`/arch -n 10000
FIXMIDFIELD = 's/	[0-9]*~*/	~/;s/~	/~-	/;s/~xx~a//'
FIXPATHS = "s;`pwd`;P;g"
goodt:	expire setup	# do NOT run this unless you know what you're doing
	$(RUN) -t explist 2>&1 | sed $(FIXPATHS) >tgood
	chmod -w tgood
goodp:	expire setup	# do NOT run this unless you know what you're doing
	$(RUN) -p explist >pgood 2>&1
	chmod -w pgood
doit:	expire		# for debugging
	echo "$(RUN) -t explist" >$@
	chmod +x $@

# the regression test proper
r:	$(THEM) $(UPACT) dircheck setup tgood pgood
	chmod +x dircheck $(THEM)
	mv active ctl/active
	$(RUN) -c explist
	: "okay, it's sane"
	$(RUN) -t explist >test.out 2>&1
	sed $(FIXPATHS) test.out | cmp - tgood
	: "okay, it seems to know what it's doing -- now, can it do it?"
	$(RUN) -p explist >test.out 2>test.stderr
	mv ctl/active active
	test ! -s test.stderr ;
	cmp test.out pgood
	test ! -f history.n ;
	cmp history.proto history.o
	egrep wont history.proto | ./dircheck arts
	egrep 'will|two|gone|three' history.proto | ./dircheck -n arts
	egrep will history.proto | ./dircheck arch
	egrep 'wont|two|gone|three' history.proto | ./dircheck -n arch
	egrep two history.proto | ./dircheck arch2
	egrep 'will|wont|gone|three' history.proto | ./dircheck -n arch2
	egrep three history.proto | ./dircheck arch3
	egrep 'will|wont|gone|two' history.proto | ./dircheck -n arch3
	test -f arts/foo/99 ;
	test -f arts/bar/ugh/99 ;
	test -f arch2/bar/99 ;
	test ! -f arts/urp/99 ;
	cmp history history.after
	: "that's it for expire, on to upact and recovact"
	$(D) ./$(UPACT)
	cmp active active.after
	$(D) ./$(UPACT)
	cmp active active.after
	$(D) ./recovact
	cmp active active.after
	sed '/^foo /s/99/09/' active.after >active
	$(D) ./recovact
	cmp active active.after
	: "and for upact, on to mkhistory"
	awk -F'	' 'NF > 2' history | sed $(FIXMIDFIELD) | sort >history.after
	rm -f history.pag history.dir
	$(D) ./mkhistory
	sort history | sed $(FIXMIDFIELD) | cmp - history.after
	test -r history.pag ;
	test -r history.dir ;
	: "success!"

# variant regression test for -r
rr:	$(THEM) $(UPACT) dircheck setup tgood pgood
	chmod +x dircheck $(THEM)
	$(RUN) -p -r explist >test.out 2>test.stderr
	test ! -s test.stderr ;
	cmp test.out pgood
	cmp history.proto history
	egrep wont history.proto | ./dircheck arts
	egrep 'will|two|gone|three' history.proto | ./dircheck -n arts
	egrep will history.proto | ./dircheck arch
	egrep 'wont|two|gone|three' history.proto | ./dircheck -n arch
	egrep two history.proto | ./dircheck arch2
	egrep 'will|wont|gone|three' history.proto | ./dircheck -n arch2
	egrep three history.proto | ./dircheck arch3
	egrep 'will|wont|gone|two' history.proto | ./dircheck -n arch3
	test -f arts/foo/99 ;
	test -f arts/bar/ugh/99 ;
	test -f arch2/bar/99 ;
	test ! -f arts/urp/99 ;

rclean:
	rm -f junk history history.pag history.dir history.o active active.tmp
	rm -f history.n* *mon.out history.proto history.after test.out doit
	rm -f active.old active.new explist lint active.after test.stderr
	rm -rf arts arch arch2 arch3 nbin ctl

clean:	rclean
	rm -f *.o expire histslash histinfo explist explist.*
	rm -f dtr updatemin

spotless:	clean	# don't run this unless you know what you're doing
	rm -f pgood tgood

dtr:	$(DTR)
	makedtr $(DTR) >$@
