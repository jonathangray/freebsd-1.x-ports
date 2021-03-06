#
# Makefile for LPC objects
#
HOST=	BSD386
DEFINES=-D$(HOST)
DEBUG=	-g -DDEBUG
CCFLAGS=$(DEFINES) $(DEBUG)
CFLAGS=	-I. -I.. -I../comp $(CCFLAGS)
CC=	gcc
DMAKE=	make
PRECOMP=../comp/a.out
CONFIG=	../lpmud
MUDLIB=	../../2.4.5

SRC=	auto.c driver.c global.c master.c user.c player.c editor.c cindent.c \
	hname.c
OBJ=	auto.o driver.o global.o master.o user.o player.o editor.o cindent.o \
	hname.o

dgd:	$(OBJ) lpc.o
	@for i in $(OBJ) lpc.o; do echo lpc/$$i; done > dgd

package:
	$(CC) $(CFLAGS) -DPKG_BIN -c $(.CURDIR)/lpc.c
	@echo lpc/lpc.o > dgd

$(SRC):	$(PRECOMP)

$(PRECOMP):
	cd ../comp; $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CCFLAGS)' \
			     a.out

auto.c:	$(MUDLIB)/dgd/lib/auto.c
	$(PRECOMP) $(CONFIG) /dgd/lib/auto.c > auto.c

driver.c: $(MUDLIB)/dgd/sys/driver.c
	$(PRECOMP) $(CONFIG) /dgd/sys/driver.c > driver.c

global.c: $(MUDLIB)/dgd/sys/global.c auto.c
	$(PRECOMP) $(CONFIG) /dgd/sys/global.c > global.c

master.c: $(MUDLIB)/obj/master.c auto.c
	$(PRECOMP) $(CONFIG) /obj/master.c > master.c

user.c:	$(MUDLIB)/dgd/std/user.c auto.c
	$(PRECOMP) $(CONFIG) /dgd/std/user.c > user.c

player.c: $(MUDLIB)/dgd/lib/player.c auto.c
	$(PRECOMP) $(CONFIG) /dgd/lib/player.c > player.c

editor.c: $(MUDLIB)/dgd/std/editor.c auto.c
	$(PRECOMP) $(CONFIG) /dgd/std/editor.c > editor.c

cindent.c: $(MUDLIB)/dgd/std/cindent.c auto.c
	$(PRECOMP) $(CONFIG) /dgd/std/cindent.c > cindent.c

hname.c: $(MUDLIB)/dgd/sys/hname.c auto.c
	$(PRECOMP) $(CONFIG) /dgd/sys/hname.c > hname.c

clean:
	rm -f dgd $(SRC) $(OBJ) lpc.o


$(OBJ) lpc.o: ../dgd.h ../config.h ../host.h ../error.h ../alloc.h ../str.h
$(OBJ) lpc.o: ../array.h ../object.h ../xfloat.h ../interpret.h ../data.h

$(OBJ) lpc.o: ../comp/csupport.h

lpc.o:	lpc.c list $(OBJ)
