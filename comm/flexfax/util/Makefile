#! smake
#	$Header: /a/cvs/386BSD/ports/comm/flexfax/util/Makefile,v 1.1 1993/08/31 23:50:00 ljo Exp $
#
# FlexFAX Facsimile Software
#
# Copyright (c) 1990, 1991, 1992, 1993 Sam Leffler
# Copyright (c) 1991, 1992, 1993 Silicon Graphics, Inc.
# 
# Permission to use, copy, modify, distribute, and sell this software and 
# its documentation for any purpose is hereby granted without fee, provided
# that (i) the above copyright notices and this permission notice appear in
# all copies of the software and related documentation, and (ii) the names of
# Sam Leffler and Silicon Graphics may not be used in any advertising or
# publicity relating to the software without the specific, prior written
# permission of Sam Leffler and Silicon Graphics.
# 
# THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
# EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
# 
# IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
# ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
# OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
# LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#
DEPTH=..
include ${DEPTH}/defs

C++FILES=\
    Array.c++ \
    Dictionary.c++ \
    Obj.c++ \
    PageSize.c++ \
    StackBuffer.c++ \
    Str.c++ \
    StrArray.c++ \
    \
    AtSyntax.c++ \
    FaxDB.c++ \
    FaxClient.c++ \
    SendFaxClient.c++ \
    TypeRules.c++ \
    Fatal.c++ \
    ${NULL}
CFILES=\
    fxassert.c \
    ${NULL}
TARGETS=libfaxutil.a faxquit faxanswer faxinfo textfmt

default all::
	@${MAKE} incdepend
	@${MAKE} targets

targets: $(TARGETS)

include ${COMMONRULES}

libfaxutil.a: ${OBJECTS}
	@rm -f $@;
	@echo "${AR} ${AROPTS} $@ ${OBJECTS}";\
	   ${AR} ${AROPTS} $@ ${OBJECTS} $(C++FILT)
	${RANLIB} $@
../util/libfaxutil.a: libfaxutil.a

faxquit: faxquit.c ${LIBS}
	${CCF} -o $@ faxquit.c ${LDFLAGS}
faxanswer: faxanswer.c ${LIBS}
	${CCF} -o $@ faxanswer.c ${LDFLAGS}
faxinfo: faxinfo.c ${LIBS}
	${CCF} -o $@ faxinfo.c ${LDFLAGS}

textfmt.o: textfmt.c++
	${C++F} -c ${C++FILE} \
	    -DFONTDIR=\"${AFMDIR}\" ${LPTOPSLCOPTS} \
	    textfmt.c++
textfmt: textfmt.o ${LIBS}
	${C++F} -o $@ textfmt.o ${LDFLAGS}

LDIRT=	checkat
checkat: checkat.o libfaxutil.a
	${C++F} -o $@ checkat.o ${LDFLAGS}

PUTSUID=${INSTALL} -m 4755 -u ${FAXUSER} -idb ${PRODUCT}.sw.server
PUTPRIV=${INSTALL} -m 4750 -u ${FAXUSER} -g ${FAXGROUP} \
    -idb ${PRODUCT}.sw.server
PUTSERV=${INSTALL} -m 755 -idb ${PRODUCT}.sw.server
PUTSSUG=${INSTALL} -m 755 -idb "${PRODUCT}.sw.server config(suggest)"

install: default
	${INSTALL} -F ${LIBEXEC} -m 755 -idb ${PRODUCT}.sw.client -O textfmt
	${INSTALL} -F ${LIBDATA} -m 444 -idb ${PRODUCT}.sw.client -O \
	    pagesizes typerules
	${PUTSUID} -F ${BIN} -O faxanswer faxquit
	${PUTPRIV} -F ${SPOOL}/bin -O faxinfo faxquit
	${PUTSERV} -F ${SPOOL}/bin -src ps2fax.gs.sh -O ps2fax.gs
	${PUTSERV} -F ${SPOOL}/bin -src ps2fax.dps.sh -O ps2fax.dps
	${PUTSSUG} -F ${SPOOL}/bin -src notify.sh -O notify
	${PUTSSUG} -F ${SPOOL}/bin -src faxrcvd.sh -O faxrcvd
	${PUTSSUG} -F ${SPOOL}/bin -src pollrcvd.sh -O pollrcvd
	${PUTSERV} -F ${SPOOL}/bin -src transcript.sh -O transcript
	${PUTSERV} -F ${SPOOL}/bin -src xferstats.sh -O xferstats
	${PUTSERV} -F ${SPOOL}/bin -src recvstats.sh -O recvstats
	${INSTALL} -F ${SPOOL}/etc -m 444 \
	    -idb ${PRODUCT}.sw.server -O dpsprinter.ps 
	${INSTALL} -F ${SPOOL}/bin -idb "${PRODUCT}.sw.server config(suggest)"\
	     -ln ${SPOOL}/bin/ps2fax.${PSIMAGER} -O ps2fax
