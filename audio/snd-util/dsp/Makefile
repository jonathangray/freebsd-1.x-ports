CC	= gcc
CFLAGS	= -O6 -m486 -funroll-loops -Wall
LD	= gcc
LDFLAGS	=

BINDIR	= /usr/local/bin

.c.o:
	$(CC) -c $(CFLAGS) -o $*.o $<

all: srec tracker

install: all
	cp srec $(BINDIR)
	rm -f $(BINDIR)/splay
	ln -s $(BINDIR)/srec $(BINDIR)/splay
	chown root $(BINDIR)/splay $(BINDIR)/srec
	chmod 755 $(BINDIR)/splay $(BINDIR)/srec
	(cd tracker; make install)

srec: recplay.o
	$(LD) $(LDFLAGS) recplay.o -o srec

tracker: .done

.done:
	(cd tracker; make )
	touch .done

clean:
	rm -f $(OBJS) recplay.o srec a.out core .done
	(cd tracker; make clean)

recplay.o: recplay.c
