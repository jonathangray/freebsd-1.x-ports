CC	= gcc
CFLAGS	= -O6 -m486 -funroll-loops -Wall
LD	= gcc
LDFLAGS	= 

BINDIR	= /usr/local/bin

OBJS	= str.o strplay.o

.c.o:
	$(CC) -c $(CFLAGS) -o $*.o $<

all: str

install: all
	install -c -m 755 str $(BINDIR)
	rm -f $(BINDIR)/str15 $(BINDIR)/str32
	ln -s $(BINDIR)/str $(BINDIR)/str15
	ln -s $(BINDIR)/str $(BINDIR)/str32

str: $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) -o str

clean:
	rm -f $(OBJS) str a.out core

str.o: str.c str.h
strplay.o: strplay.c str.h
