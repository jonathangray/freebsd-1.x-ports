# makefile for C news inews and relatives

# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# =()<NEWSCTL = @<NEWSCTL>@>()=
NEWSCTL = /usr/lib/news
# workaround for System V make bug
SHELL = /bin/sh

CMDS=actflag defaults defhdrs.awk inews lines modroute injnews pnews stripinvis submit.relay tear
BIN=/bin
NPROC=2
# -DVOID=int for libc.h & old lint libraries
#DEFINES= -I../include -I../relay -I. -DVOID=int
DEFINES= -I../h -I../relay -I. -DVOID=int
#CC=CC +V
#CC=gcc -ansi -pedantic -Wall -S
COPTS= -O # -pg -g -O
CFLAGS=$(DEFINES) $(COPTS)
DBM = # -ldbm
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
SRC=# ...
OBJ=# ...
FILES=$(NONCFILES) $(CFILES)
NONCFILES= TODO* README ads/README ads/[0-9]* \
	inews injnews pnews tear defhdrs.awk makefile
CFILES= ../include/*.h  $(SRC)

all: makefile $(CMDS)
	chmod +x $(CMDS)

lint: lint.defaults lint.actflag

mkfile: makefile
	sed '/mkfile/d' makefile | mkconv | sed 's/make/mk/g' >$@

actflag: actflag.o ../relay/active.o $(LIBOBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) actflag.o ../relay/active.o $(PRE) $(LIBS) $(LIBOBJS) $(POST) -o $@
lint.actflag: $(SRC)
	$(LINT) $(LINTFLAGS) actflag.c $(LLIBS) 2>&1 | $(LINTFILT)
lint-p.actflag: $(SRC)
	$(LINT) $(LINTFLAGS) -p actflag.c $(LLIBS) 2>&1 | $(LINTFILT)

defaults: defaults.o fullname.o getmypwent.o intcode.o $(LIBOBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) defaults.o fullname.o getmypwent.o intcode.o $(PRE) $(LIBS) $(LIBOBJS) $(POST) -o $@
lint.defaults: $(SRC)
	$(LINT) $(LINTFLAGS) defaults.c fullname.c getmypwent.c intcode.c $(LLIBS) 2>&1 | $(LINTFILT)
lint-p.defaults: $(SRC)
	$(LINT) $(LINTFLAGS) -p defaults.c fullname.c getmypwent.c intcode.c $(LLIBS) 2>&1 | $(LINTFILT)

stripinvis: mkstripinvis
	chmod +x mkstripinvis
	./mkstripinvis >$@
	chmod +x $@

# this botch is for backward compatibility
newsinstall: $(NEWSBIN)/inject/inews
	rm -f $(NEWSCTL)/inews
	ln $(NEWSBIN)/inject/inews $(NEWSCTL)/inews 2>/dev/null || cp inews $(NEWSCTL)

# bininstall: make directories, install programs
bininstall: install
install $(NEWSBIN)/inject/inews: $(CMDS)
	-mkdir $(NEWSBIN)/inject
	cp $(CMDS) $(NEWSBIN)/inject
	rm -f $(BIN)/inews $(BIN)/injnews
	ln $(NEWSBIN)/inject/inews $(BIN)/inews 2>/dev/null || cp inews $(BIN)
	ln $(NEWSBIN)/inject/injnews $(BIN)/injnews 2>/dev/null || cp injnews $(BIN)

cmp:
	for f in $(CMDS) ; do cmp $(NEWSBIN)/inject/$$f $$f ; done
	cmp $(BIN)/inews inews
	cmp $(NEWSCTL)/inews inews

check:
	for f in $(CMDS) ; do cmp $(NEWSBIN)/inject/$$f $$f || true ; done
	cmp $(BIN)/inews inews || true
	cmp $(NEWSCTL)/inews inews || true

TODO.grep: TODO
	-egrep TODO ../include/*.h *.h *.c * | tr -s " \11" " " >$@
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
	rm -rf core a.out *.o regress/tmp actflag defaults

r: $(CMDS)
	chmod +x regress/regress
	(cd regress; ./regress)
	
# header dependencies follow
