TROFF=groff
PRINTER= lpr
FILES= guide

all: guide.ps index.ps toc.ps

guide.ps index.raw: $(FILES) ihave.pic
	soelim $(FILES) | tbl | $(TROFF) -ms >guide.ps 2>index.raw

preview index.raw: $(FILES) ihave.pic
	soelim $(FILES) | tbl | xproof -ms 2>index.raw

guide.out: $(FILES)
	soelim guide | tbl | nroff -Tdumb -ms - >$@

#ihave.pic.in:  ihave.xpic
#	x2pic <ihave.xpic >$@

ihave.pic:  ihave.pic.in
	pic ihave.pic.in >$@

clean:
	rm -f *.pic *.ps index.raw

toc.ps: index.raw toc.start toc.end toc.awk
	(cat toc.start; toc.awk index.raw; cat toc.end) | tbl | $(TROFF) -ms >$@

# Copyright 1985, Massachusetts Institute of Technology
# based on Makefile for the Xlib document.

index.ps: index.raw fixindex.awk block.awk indexmacros.t end.t
	sed -n '/^index:/s///p' index.raw | sort -ft: +1 -3 +0n -1 | awk -f fixindex.awk | awk -f block.awk | $(TROFF) -ms indexmacros.t - end.t >$@
