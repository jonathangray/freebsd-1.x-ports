LIB	=/usr/local/lib/tex82/localmacros

LAPARTS	= prepictex.tex pictex.tex postpictex.tex
TEXPARTS = latexpicobjs.tex pictex.tex

all:	piclatex.sty $(TEXPARTS)

install: all
	install -c -m 444 latexpicobjs.tex $(LIB)
	install -c -m 444 pictex.tex $(LIB)
	install -c -m 444 prepictex.tex $(LIB)
	install -c -m 444 postpictex.tex $(LIB)
	install -c -m 444 piclatex.sty $(LIB)

piclatex.sty:
	cat $(LAPARTS)  \
	| sed -e '/^%/d' -e '/^[ 	]*$$/d' \
	| cat -s > piclatex.sty

$(LAPARTS) $(TEXPARTS):
	co -q $@

