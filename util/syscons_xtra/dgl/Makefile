#  Copyright (C) 1991 By DeepCore Technologies
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 1, or any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#      DeepCore Technologies
#	Att: Søren Schmidt 	Email:	sos@kmd-ac.dk
#	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
#	DK9210 Aalborg SO	Phone:  +45 9814 8076

CFLAGS=-g
CC=gcc 
LIBOBJ= main.o driver.o ddfuncs.o difuncs.o bitmap.o widget.o text.o 

all:	libdgl.a demo demo1 demo2 tpic gv # dissolve 

demo: 	demo.c dgl.h compiler.h macros.h libdgl.a
	$(CC) $(CFLAGS) -o demo demo.c libdgl.a 

demo1: 	demo1.c dgl.h compiler.h macros.h libdgl.a
	$(CC) $(CFLAGS) -o demo1 demo1.c libdgl.a -lm 

demo2: 	demo2.c dgl.h compiler.h macros.h libdgl.a
	$(CC) $(CFLAGS) -o demo2 demo2.c libdgl.a -lm 

tpic: 	tpic.c dgl.h compiler.h macros.h libdgl.a
	$(CC) $(CFLAGS) -o tpic tpic.c libdgl.a

dissolve: dissolve.c dgl.h compiler.h macros.h libdgl.a
	$(CC) $(CFLAGS) -o dissolve dissolve.c libdgl.a

gv: 	gv.c dgl.h compiler.h macros.h libdgl.a
	$(CC) $(CFLAGS) -o gv gv.c libdgl.a 

libdgl.a: $(LIBOBJ) 
	ar rv libdgl.a $(LIBOBJ)
	ranlib libdgl.a

install:
	cp libdgl.a /usr/local/lib
	ranlib /usr/local/lib/libdgl.a

clean: 
	-rm -f *.o *.a core demo demo1 demo2 tpic dissolve gv sharfil 

shar:	
	shar README Makefile *.h *.c *.s > sharfil

print:	
	fprn -p12 README Makefile *.h *.c *.s 

prototypes.p: main.c ddfuncs.c difuncs.c bitmap.c widget.c text.c
	cproto -U__GNUC__ -p main.c difuncs.c ddfuncs.c bitmap.c widget.c text.c > prototypes.p

ddfuncs.o: ddfuncs.c dgl.h

difuncs.o: difuncs.c dgl.h

main.o: main.c dgl.h driver.h

driver.o: driver.c driver.h stdpalette.h modetables.h dgl.h

bitmap.o: bitmap.c dgl.h

text.o:	text.c dgl.h

widget.o: widget.c dgl.h

portio.o: portio.s

dgl.h: compiler.h macros.h prototypes.p

