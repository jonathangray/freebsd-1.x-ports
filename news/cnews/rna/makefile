# australian readnews makefile
# =()<NEWSARTS = @<NEWSARTS>@>()=
NEWSARTS = /usr/spool/news
# =()<NEWSBIN = @<NEWSBIN>@>()=
NEWSBIN = /usr/lib/newsbin
# =()<NEWSCTL = @<NEWSCTL>@>()=
NEWSCTL = /usr/lib/news
# workaround for System V make bug
SHELL = /bin/sh

DEFINES=
COPTS=-O
CFLAGS=$(COPTS) $(DEFINES) -I../include
INSTALL=:# install bigpdp
NFLAG =
LINT = lint -ha $(DEFINES)
I=/usr/include
C=/lib/libc.a
LIBS=../libcnews.a bsearch.o
BIN=/bin
BINDIR=$(BIN)

COMMANDS = postnews uurec readnews uusend expire postgroup
FILES = help
LCOMMANDS = postnews.lint uurec.lint readnews.lint uusend.lint expire.lint
# PFILES = header.c postnews.c funcs.c active.c history.c maketime.c
PFILES = header.c funcs.c active.c history.c maketime.c
RFILES = header.c readnews.c funcs.c active.c newsrc.c history.c maketime.c
ROFILES= header.o readnews.o funcs.o active.o newsrc.o
EFILES = expire.c funcs.c active.c

.c.lint:
	$(LINT) $< > $@

.c:
	$(CC) $(CFLAGS) $*.c $(NFLAG) -o $(@F)
	$(INSTALL) $* bin 711 $@

.sh:
	$(INSTALL) -c $*.sh bin 755 $@

all: readnews checknews postnews
	chmod +x checknews postnews

readnews: $(ROFILES) defs.h $(LIBS)
	$(CC) $(CFLAGS) $(LDFLAGS) $(ROFILES) $(PRE) $(LIBS) $(POST) -o $@

newsinstall:
	: "install all control files; don't overwrite any!"
	-if test ! -r $(NEWSCTL)/readnews.help; then cp news.help $(NEWSCTL)/readnews.help; fi

# bininstall: make directories, install programs
bininstall:	readnews postnews checknews
	cp readnews $(BINDIR)
	cp checknews $(BINDIR)
	cp postnews $(BINDIR)
	cp postnews $(NEWSBIN)/inject

cmp:	readnews checknews postnews
	cmp $(BINDIR)/readnews readnews
	cmp $(BINDIR)/checknews checknews
	cmp $(BINDIR)/postnews postnews
	cmp $(NEWSBIN)/inject/postnews postnews

check:	readnews checknews postnews
	cmp $(BINDIR)/readnews readnews || true
	cmp $(BINDIR)/checknews checknews || true
	cmp $(BINDIR)/postnews postnews || true
	cmp $(NEWSBIN)/inject/postnews postnews || true

lint: $(LCOMMANDS)

help: $(NEWSCTL)/help
$(NEWSCTL)/help: news.help
	$(INSTALL) -c news.help news 644 $(NEWSCTL)/help

lib/lib.a:
	: if you do not have ranlib, you will need to edit lib/makefile
	(cd lib; make)

$(ROFILES): defs.h

# postnews: $(BINDIR)/postnews
# $(BINDIR)/postnews: $(PFILES) defs.h
# 	$(CC) $(CFLAGS) $(PFILES) -o postnews
# 	$(INSTALL) - postnews news 6711 $(BINDIR)/postnews

postgroup: $(BINDIR)/postgroup
$(BINDIR)/postgroup: postgroup.sh

uurec: $(NEWSCTL)/uurec
$(NEWSCTL)/uurec: uurec.c defs.h

uusend: $(NEWSCTL)/uusend
$(NEWSCTL)/uusend: defs.h uusend.c

expire: $(NEWSCTL)/expire
$(NEWSCTL)/expire: $(EFILES) defs.h
	$(CC) $(CFLAGS) $(EFILES) $(NFLAG) -o expire
	$(INSTALL) expire news 700 $(NEWSCTL)/expire

postnews.lint: $(PFILES)
	$(LINT) $(PFILES) > postnews.lint

readnews.lint: $(RFILES)
	$(LINT) $(RFILES) > readnews.lint

expire.lint: $(EFILES)
	$(LINT) $(EFILES) > expire.lint

uurec.lint: uurec.c defs.h
uusend.lint: uusend.c defs.h

.FINISH:
	rm -s *.o
clean:
	rm -f *.o core a.out readnews
