CFLAGS = -O
LINTFLAGS = -abchnpux
CSUBS = Expand.c Match.c subs.c
OSUBS = Expand.o Match.o subs.o
B =

default: all

all: texexpand detex texeqn texmatch

texexpand: texexpand1.o $(OSUBS)
	cc $(CFLAGS) -o $(B)texexpand texexpand1.o $(OSUBS)

detex: detex1.o DeTeX.o $(OSUBS)
	cc $(CFLAGS) -o $(B)detex detex1.o DeTeX.o $(OSUBS)

texeqn: texeqn1.o Eqn.o $(OSUBS)
	cc $(CFLAGS) -o $(B)texeqn texeqn1.o Eqn.o $(OSUBS)

texmatch: texmatch1.o $(OSUBS)
	cc $(CFLAGS) -o $(B)texmatch texmatch1.o $(OSUBS)

lint:
	lint $(LINTFLAGS) texexpand1.c $(CSUBS) > texexpand.lnt
	lint $(LINTFLAGS) detex1.c DeTeX.c $(CSUBS) > detex.lnt
	lint $(LINTFLAGS) texeqn1.c Eqn.c $(CSUBS) > texeqn.lnt
	lint $(LINTFLAGS) texmatch1.c $(CSUBS) > texmatch.lnt
	
clean:
	/bin/rm -f *.o texexpand detex texeqn texmatch core *junk*
