CC	= gcc
CFLAGS	= -O6 -m486 -funroll-loops -Wall
LD	= gcc
LDFLAGS	=

BINDIR	= /usr/local/bin

.c.o:
	$(CC) -c $(CFLAGS) -o $*.o $<

all: srec stracker_player

install: all
	cp srec $(BINDIR)
	rm -f $(BINDIR)/splay
	ln -s $(BINDIR)/srec $(BINDIR)/splay
	chown root $(BINDIR)/splay $(BINDIR)/srec
	chmod 755 $(BINDIR)/splay $(BINDIR)/srec
	(cd stracker_player; make install)

srec: recplay.o
	$(LD) $(LDFLAGS) recplay.o -o srec

stracker_player: .done

.done:
	(cd stracker_player; make )
	touch .done

clean:
	rm -f $(OBJS) recplay.o srec a.out core .done
	(cd stracker_player; make clean)

recplay.o: recplay.c
