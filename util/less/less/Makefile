PROG=	less
LDADD+= -ltermcap
HELPFILE =		${LIBDIR}/less.hlp
MAN1=	less.1

CFLAGS+= -DHELPFILE=\"${HELPFILE}\" -DSHELL_CMD -DTAGS

BINDIR=	/usr/bin
LIBDIR=	/usr/lib

SRCS=	ch.c cmdbuf.c command.c decode.c help.c input.c \
	line.c linenum.c main.c edit.c option.c optfunc.c \
	opttbl.c os.c charset.c filename.c lsystem.c output.c \
	position.c ifile.c brac.c forwback.c jump.c search.c \
	mark.c prompt.c screen.c signal.c tags.c ttyin.c version.c

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/less.hlp \
	    ${HELPFILE}

.include <bsd.prog.mk>
