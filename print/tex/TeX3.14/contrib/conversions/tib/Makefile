CC = cc
CFLAGS = -O
DESTDIR = DDDDD

.c.o:
	$(CC) -c $(CFLAGS) $*.c

DEXo        = tibdex.o streams.o  makekey.o
DEXc        = tibdex.c streams.c  makekey.c
LOOKo       = tiblook.o locate.o seek.o streams.o makekey.o
LOOKc       = tiblook.c locate.c seek.c streams.c makekey.c
TIBo        = tib.o tibargs.o locate.o seek.o streams.o makekey.o 
TIBc        = tib.c tibargs.c locate.c seek.c streams.c makekey.c 
LISTo       = tiblist.o tibargs.o streams.o makekey.o
LISTc       = tiblist.c tibargs.c streams.c makekey.c

SRCS = \
	tib.c \
	tibargs.c \
	seek.c \
	tib.h \
	tibdex.c \
	tiblist.c \
	locate.c \
	tiblook.c \
	makekey.c \
	streams.c

all:	tibdex tiblook tiblist tib

install: all
	install -s -c tibdex $(DESTDIR)/tibdex
	install -s -c tiblook $(DESTDIR)/tiblook
	install -s -c tiblist $(DESTDIR)/tiblist
	install -s -c tib $(DESTDIR)/tib

tibdex:  $(DEXo)
	$(CC) $(CFLAGS)  $(DEXo)   -o tibdex

tiblook: $(LOOKo)
	$(CC) $(CFLAGS)  $(LOOKo)  -o tiblook

tib: $(TIBo)
	$(CC) $(CFLAGS)  $(TIBo)  -o tib

tiblist: $(LISTo)
	$(CC) $(CFLAGS)  $(LISTo)  -o tiblist

tib.o:		tib.h
tibargs.o:	tib.h
tiblist.o:	tib.h
tibdex.o:       tib.h
locate.o:       tib.h
makekey.o:	tib.h

manual: tib.cat

tib.cat:
	nroff -h -man tib.man>tib.cat

clean: 
	rm -f tib.o tibargs.o tiblist.o tibdex.o tiblook.o
	rm -f locate.o makekey.o streams.o seek.o 

veryclean: clean
	rm -f tib tibdex tiblist tiblook

