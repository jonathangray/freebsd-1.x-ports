#! smake
#	$Header: /a/cvs/386BSD/ports/comm/flexfax/iv/Makefile,v 1.1 1993/08/31 23:43:38 ljo Exp $
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
    Dispatch/dispatcher.c++ \
    Dispatch/iohandler.c++ \
    OS/memory.c++ \
    InterViews/regexp.c++ \
    ${NULL}
TARGETS=libivhack.a

LC++INCS=-I.

default all::
	@${MAKE} incdepend
	@${MAKE} targets

targets: $(TARGETS)

include ${COMMONRULES}

OBJECTS=\
    dispatcher.o \
    iohandler.o \
    memory.o \
    regexp.o \
    ${NULL}

libivhack.a: ${OBJECTS}
	@rm -f $@;
	@echo "${AR} ${AROPTS} $@ ${OBJECTS}"; ${AR} ${AROPTS} $@ ${OBJECTS} ${C++FILT}
	${RANLIB} $@

dispatcher.o: Dispatch/dispatcher.c++
	${C++F} ${C++FILE} -c Dispatch/dispatcher.c++
iohandler.o: Dispatch/iohandler.c++
	${C++F} ${C++FILE} -c Dispatch/iohandler.c++
memory.o: OS/memory.c++
	${C++F} ${C++FILE} -c OS/memory.c++
regexp.o: InterViews/regexp.c++
	${C++F} ${C++FILE} -c InterViews/regexp.c++

install: default
