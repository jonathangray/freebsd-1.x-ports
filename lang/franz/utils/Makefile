# $Header: /a/cvs/386BSD/ports/lang/franz/utils/Makefile,v 1.1 1994/03/18 16:26:39 jkh Exp $
# $Locker:  $

LibDir = /usr/lib/lisp
AllSrc = Makefile append.c tackon.c

AllObj = ${LibDir}/append ${LibDir}/tackon

all: ${LibDir}/append ${LibDir}/tackon

${LibDir}/append: append.c
	cc -O -o ${LibDir}/append  -I../franz/h append.c

${LibDir}/tackon: tackon.c
	cc -O -o ${LibDir}/tackon -I../franz/h tackon.c

copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))


scriptcatall: ${AllSrc}
	@../scriptcat . utils ${AllSrc}


clean:
	rm ${AllObj}
