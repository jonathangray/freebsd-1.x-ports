# Makefile for the Men3wn library.

# @(#)Makefile	1.11 20/10/91 (UKC)

# Optional multi-architecture compilation support
D = ${OBJDIR}${SLASH}
S = ${SRCDIR}${SLASH}

LIBNAME = Men3wn
HDRNAME = menu3
LIBDIRNAME = med3lib
LINTLIBS = -lwn

TESTSRCS =
LIBSRCS = marea.c mclose.c mcreate.c mdisplay.c mdup.c mfmodes.c minfo.c minsert.c mnewcap.c mopen.c mperror.c msize.c mstdfont.c wdbyte.c
OBJS = $Dmarea.o $Dmclose.o $Dmcreate.o $Dmdisplay.o $Dmdup.o $Dmfmodes.o $Dminfo.o $Dminsert.o $Dmnewcap.o $Dmopen.o $Dmperror.o $Dmsize.o $Dmstdfont.o $Dwdbyte.o $Dsccsdata.o
HDRS = menu3.h menu_priv.h sccsdata.h wdbyte.h 

DOCS = 

SCCSCHECK_DIRS = ../libx11wn

# Begin common part of Makefile
target: $Dlib${LIBNAME}.a

INCLUDE = /usr/include/local
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
	ddeps ${DEPS_DEVELHDRS} ${SRCS} sccsdata.c > t_deps

# End common part of Makefile

# Machine generated .o file dependencies
$Dmarea.o: marea.c menu3.h menu_priv.h wdbyte.h 
	${CCLINE}marea.c
$Dmclose.o: mclose.c menu3.h menu_priv.h 
	${CCLINE}mclose.c
$Dmcreate.o: mcreate.c menu3.h menu_priv.h 
	${CCLINE}mcreate.c
$Dmdisplay.o: mdisplay.c menu3.h menu_priv.h sccsdata.h wdbyte.h 
	${CCLINE}mdisplay.c
$Dmdup.o: mdup.c menu3.h menu_priv.h 
	${CCLINE}mdup.c
$Dmfmodes.o: mfmodes.c menu3.h menu_priv.h 
	${CCLINE}mfmodes.c
$Dminfo.o: minfo.c menu3.h menu_priv.h 
	${CCLINE}minfo.c
$Dminsert.o: minsert.c menu3.h menu_priv.h 
	${CCLINE}minsert.c
$Dmnewcap.o: mnewcap.c menu3.h menu_priv.h 
	${CCLINE}mnewcap.c
$Dmopen.o: mopen.c menu3.h menu_priv.h 
	${CCLINE}mopen.c
$Dmperror.o: mperror.c menu3.h menu_priv.h 
	${CCLINE}mperror.c
$Dmsize.o: msize.c menu3.h menu_priv.h 
	${CCLINE}msize.c
$Dmstdfont.o: mstdfont.c menu3.h 
	${CCLINE}mstdfont.c
$Dwdbyte.o: wdbyte.c menu3.h menu_priv.h wdbyte.h 
	${CCLINE}wdbyte.c
$Dsccsdata.o: sccsdata.c sccsdata.h 
	${CCLINE}sccsdata.c
