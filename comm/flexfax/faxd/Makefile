#! smake
#	$Header: /a/cvs/386BSD/ports/comm/flexfax/faxd/Makefile,v 1.1 1993/08/31 23:42:46 ljo Exp $
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
	FaxModem.c++ \
	HDLCFrame.c++ \
	ModemConfig.c++ \
	\
	Everex.c++ \
	EverexPoll.c++ \
	EverexRecv.c++ \
	EverexSend.c++ \
	Class1.c++ \
	Class1Poll.c++ \
	Class1Send.c++ \
	Class1Recv.c++ \
	Class2.c++ \
	Class2Params.c++ \
	Class2Poll.c++ \
	Class2Recv.c++ \
	Class2Send.c++ \
	\
	DialRules.c++ \
	FaxServer.c++ \
	FaxPoll.c++ \
	FaxRecv.c++ \
	FaxSend.c++ \
	FaxRecvInfo.c++ \
	FaxRequest.c++ \
	FaxMachineInfo.c++ \
	FaxMachineLog.c++ \
	RegExArray.c++ \
	UUCPLock.c++ \
	\
	Getty.c++ \
	${SYSGETTY} \
	\
	faxServerApp.c++ \
	${NULL}
TARGETS=faxd

default all::
	@${MAKE} incdepend
	@${MAKE} targets

targets: $(TARGETS)

include ${COMMONRULES}

faxd:	${OBJECTS} ${LIBS}
	${C++F} -o $@ ${OBJECTS} ${LDFLAGS}

dialtest: dialtest.o DialRules.o RegExArray.o ${LIBS}
	${C++F} -o $@ dialtest.o DialRules.o RegExArray.o ${LDFLAGS}

UUCPLock.o: UUCPLock.c++
	${C++F} -c ${C++FILE} \
	    -DUUCP_LOCKTYPE=${UUCP_LOCKTYPE} \
	    -DUUCP_LOCKMODE=${UUCP_LOCKMODE} \
	    -DUUCP_LOCKDIR=\"${UUCP_LOCKDIR}\" \
	    UUCPLock.c++

install: default
	${INSTALL} -F ${USRETC} -u ${FAXUSER} -m 4755 \
	    -idb ${PRODUCT}.sw.server -O faxd
