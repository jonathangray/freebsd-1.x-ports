#  Copyright (C) 1992, 1993 Søren Schmidt
#
#  This program is free software; you may redistribute it and/or 
#  modify it, provided that it retain the above copyright notice 
#  and the following disclaimer.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#
#	Søren Schmidt 		Email:	sos@kmd-ac.dk
#	Tritonvej 36		UUCP:	...uunet!dkuug!kmd-ac!sos
#	DK9210 Aalborg SO	Phone:  +45 9814 8076

CC=	gcc -pipe

all:	80x25 80x50 blank keyrate setfkey mapkbd getkbdmap setfont kbfiles \
	setmode getmode switchto reldisp

install: all
	install -s -m 755 80x25 80x50 blank keyrate setfkey mapkbd getkbdmap /usr/local/bin
	install -s -m 755 setfont setmode getmode switchto reldisp /usr/local/bin
	-mkdir /usr/local/lib/syscons
	cp *.map /usr/local/lib/syscons
	cp ../fonts/* /usr/local/lib/syscons

80x25:  	80x25.c
	$(CC) -o 80x25 80x25.c

80x50:  	80x50.c
	$(CC) -o 80x50 80x50.c

blank:  	blank.c
	$(CC) -o blank blank.c

keyrate:	keyrate.c
	$(CC) -o keyrate keyrate.c

setfkey:	setfkey.c
	$(CC) -o setfkey setfkey.c

mapkbd:		mapkbd.c
	$(CC) -o mapkbd mapkbd.c

getkbdmap:	getkbdmap.c
	$(CC) -o getkbdmap getkbdmap.c

setfont:	setfont.c
	$(CC) -o setfont setfont.c

setmode:	setmode.c
	$(CC) -o setmode setmode.c

getmode:	getmode.c
	$(CC) -o getmode getmode.c

switchto:	switchto.c
	$(CC) -o switchto switchto.c

reldisp:	reldisp.c
	$(CC) -o reldisp reldisp.c

kbfiles: 
	@for f in ../kbdmaps/*;\
	do \
	echo $(CC) -o mkfil -I../kbdmaps -DFIL=\<`basename $$f`\> mkkbdfil.c;\
	$(CC) -o mkfil -I../kbdmaps -DFIL=\<`basename $$f`\> mkkbdfil.c;\
	echo ./mkfil `basename $$f`.map;\
	./mkfil `basename $$f`.map;\
	done
	rm mkfil

clean:
	rm -f 80x25 80x50 blank keyrate setfkey mapkbd getkbdmap \
	      setmode getmode switchto reldisp setfont *.o *.map

