# Makefile for the obj library.

# @(#)Makefile	1.11 29/6/92 (UKC)

# Optional multi-architecture support
D = ${OBJDIR}${SLASH}
S = ${SRCDIR}${SLASH}

LIBNAME = obj
HDRNAME = ${LIBNAME}
LIBDIRNAME = ${LIBNAME}
LINTLIBS = -lwn

TESTSRCS =
LIBSRCS = o_chds.c o_def.c o_disp.c o_globals.c o_lptrs.c \
	  o_mkobj.c o_sel.c o_text.c \
	  fed.c newtb.c mhole.c

OBJS =   $Do_chds.o $Do_def.o $Do_disp.o $Do_globals.o $Do_lptrs.o \
	 $Do_mkobj.o $Do_sel.o $Do_text.o \
	 $Dfed.o $Dnewtb.o $Dmhole.o $Dsccsdata.o

HDRS = \
	fed.h mhole.h newtb.h o_disp.h o_lptrs.h o_mkobj.h o_priv.h \
	o_sel.h obj.h sccsdata.h 

DOCS = doc/tech.ms

SCCSCHECK_DIRS = ../libx11wn

# Begin common part of Makefile
target: $Dlib${LIBNAME}.a

INCLUDE = /usr/local/include
LIB = /usr/local/lib

# Flags for cc. CCFLAGS is read from the environment
DEPS_DEVELHDRS = -Idevelhdrs
DEVELHDRS = -I$Sdevelhdrs
CFLAGS = ${CCFLAGS} ${DEVELHDRS} ${INCLUDEDIRS} ${ARCH_CFLAGS} ${MORE_CFLAGS}

CCLINE = ${IN_OBJDIR} ${CC} -c ${CFLAGS} $S

SRCS = ${LIBSRCS} ${TESTSRCS}
FILES = ${SRCS} ${HDRS} ${DOCS} Makefile

# Use $D/. so we don't get a null target when $D is undefined.
# We say sh -c to avoid exit status problems from the || construct.
$D/.:
	-@(IFS=/; for i in $D; do test -d $$i || mkdir $$i; cd $$i; done)

$Dtest${LIBNAME}: $D/. $Dlib${LIBNAME}.a $Dtest${LIBNAME}.o ${TESTLIBDEPS}
	${CC} ${CFLAGS} ${LINKFLAGS} -o $Dtest${LIBNAME} $Dtest${LIBNAME}.o $Dlib${LIBNAME}.a ${TESTLIBS}

$Dlib${LIBNAME}.a: $D/. ${OBJS}
	rm -f $@
	ar cq $@ ${OBJS}
	${IN_OBJDIR} ranlib lib${LIBNAME}.a

all: $Dlib${LIBNAME}.a

tags: ${SRCS}
	ctags -tw ${SRCS} ${HDRS}

list:
	@echo README ${FILES} sccsdata.c | tr ' ' '\012'

count:
	wc ${HDRS} ${SRCS}

CHECKSCCS = sccsinfo ${SCCSCHECK_DIRS} ${FILES} && newer sccsdata.c ${FILES}

checksccs:
	@${CHECKSCCS}

sccssnapshot:
	@mksccs -changefile CHANGES -wh -c t_snapshot.c -h t_snapshot.h ${LIBNAME} ${FILES}

sccsfile:
	@sccsinfo ${SCCSCHECK_DIRS} ${FILES}
	@checklist
	@newer CHANGES ${FILES}
	@sccs edit sccsdata.c
	@mksccs -changefile CHANGES -wh ${LIBNAME} ${FILES}
	@sccs delget sccsdata.c


install: $Dinstall

$Dinstall: ${LIB}/lib${LIBNAME}.a ${INCLUDE}/${HDRNAME}.h

${LIB}/lib${LIBNAME}.a: $Dlib${LIBNAME}.a
	${CHECKSCCS}
	install -c -m 644 -o root $? $@
	cd ${LIB}; ranlib $@

${INCLUDE}/${HDRNAME}.h: ${HDRNAME}.h
	${CHECKSCCS}
	install -c -m 644 -o root $? $@

clean:
	rm -f ${OBJS} $Dlib${LIBNAME}.a $Dllib-l${LIBNAME}.ln core

veryclean: clean
	rm -f tags
	sccs clean

lintf:
	lint ${LINTFLAGS} ${ARCH_CFLAGS} ${SRCS} ${LINTLIBS} | .lintf > t_lintf

llib: $Dllib-l${LIBNAME}.ln

$Dllib-l${LIBNAME}.ln: ${LIBSRCS}
	lint -C${LIBNAME} ${ARCH_CFLAGS} ${LIBSRCS}
	mv llib-l${LIBNAME}.ln $@

install_lint: ${LIB}/lint/llib-l${LIBNAME}.ln

${LIB}/lint/llib-l${LIBNAME}.ln: $Dllib-l${LIBNAME}.ln
	${CHECKSCCS}
	install -c -m 644 -o root $? $@

deps:
	@deps ${DEPS_DEVELHDRS} ${SRCS} sccsdata.c

# End common part of Makefile

# Machine generated .o file dependencies
$Do_chds.o: o_chds.c obj.h o_priv.h o_disp.h o_sel.h o_lptrs.h o_mkobj.h \
  sccsdata.h 
	${CCLINE}o_chds.c
$Do_def.o: o_def.c obj.h o_priv.h o_mkobj.h 
	${CCLINE}o_def.c
$Do_disp.o: o_disp.c obj.h o_priv.h o_sel.h o_disp.h o_lptrs.h o_mkobj.h 
	${CCLINE}o_disp.c
$Do_globals.o: o_globals.c obj.h o_priv.h 
	${CCLINE}o_globals.c
$Do_lptrs.o: o_lptrs.c obj.h o_priv.h o_mkobj.h o_lptrs.h o_sel.h 
	${CCLINE}o_lptrs.c
$Do_mkobj.o: o_mkobj.c obj.h o_priv.h o_mkobj.h o_sel.h o_lptrs.h 
	${CCLINE}o_mkobj.c
$Do_sel.o: o_sel.c obj.h o_priv.h o_mkobj.h o_lptrs.h o_sel.h 
	${CCLINE}o_sel.c
$Do_text.o: o_text.c obj.h o_priv.h 
	${CCLINE}o_text.c
$Dfed.o: fed.c fed.h 
	${CCLINE}fed.c
$Dnewtb.o: newtb.c newtb.h 
	${CCLINE}newtb.c
$Dmhole.o: mhole.c mhole.h 
	${CCLINE}mhole.c
$Dsccsdata.o: sccsdata.c sccsdata.h 
	${CCLINE}sccsdata.c
