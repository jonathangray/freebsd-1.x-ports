
.if !exists(${.CURDIR}/../Makefile.inc)
error:
	@echo
	@echo "                      You MUST link/copy"
	@echo
	@echo "       ../Makefile.inc.NetBSD    or   ../Makefile.inc.FreeBSD"
	@echo
	@echo "             to ../Makefile.inc before you can proceed !"
	@echo
.else

PROG	=	ispcvt
IBINDIR	=	/usr/sbin
MAN8	=	ispcvt.${MAN8EXT}

.include <bsd.prog.mk>

$(PROG):	$(INCDIR)/pcvt_ioctl.h

beforeinstall:
	@if [ ! -d $(MANDIR)8 ] ; then		\
	echo making $(MANDIR)8; mkdir $(MANDIR)8; fi

install:	maninstall
	install -c ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${PROG} ${IBINDIR}

.endif
