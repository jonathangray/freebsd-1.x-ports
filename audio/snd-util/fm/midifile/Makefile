CFLAGS = -g

mftext : midifile.o mftext.o crack.o
	cc midifile.o mftext.o crack.o -o mftext

mfcheck : midifile.o mfcheck.o
	cc midifile.o mfcheck.o -o mfcheck

mfstrings : midifile.o mfstrings.o
	cc midifile.o mfstrings.o -o mfstrings

mf1to0 : midifile.o mf1to0.o
	${CC} ${CFLAGS} $@.c midifile.o -o $@

mfwrite_ex : midifile.o mfwrite_ex.o
	cc midifile.o mfwrite_ex.o -o mfwrite_ex

all : mftext mfcheck mfstrings mf1to0 midifile.man

midifile.man : midifile.3
	nroff -man -Tlp midifile.3 | col -b > midifile.man

clean :
	rm -f mftext mfcheck mfstrings mf1to0 mfwrite_ex *.o midifile.man

lint :
	lint midifile.c mf1to0.c

test : mftext
	for i in /usr/ports/audio/music/midi/example*.mid ; \
	do \
		echo $$i ; \
		mftext $$i > /dev/null ; \
		if [ $$? -ne 0 ] ; then echo "Mftext of $$i failed!" ; fi ; \
	done
