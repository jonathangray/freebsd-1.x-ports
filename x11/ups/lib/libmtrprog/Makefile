# @(#)Makefile	1.29 26/7/92 (UKC)

# Makefile for the mtrutil library.

# Optional multi-architecture support
D = ${OBJDIR}${SLASH}
S = ${SRCDIR}${SLASH}

LIBNAME = mtrprog
HDRNAME = mtrprog
LIBDIRNAME = mtrprog
LINTLIBS = 

LIBSRCS = \
	e_close.c is_number.c memxxx.c query_user.c \
	rm_rf.c serverhost.c so.c strcache.c regex.c \
	swap_longs.c appltofl.c fmtostr.c \
	fttostr.c fopnew.c strtobsz.c mtrprog_vers.c

OBJS = \
	$De_close.o $Dis_number.o $Dmemxxx.o $Dquery_user.o \
	$Drm_rf.o $Dserverhost.o $Dso.o $Dstrcache.o $Dregex.o \
	$Dswap_longs.o $Dappltofl.o $Dfmtostr.o \
	$Dfttostr.o $Dfopnew.o $Dstrtobsz.o $Dmtrprog_vers.o \
	$Dsccsdata.o

HDRS = \
	alloc.h genmergesort.h ifdefs.h regex.h sccsdata.h \
	serverhost.h so.h strcache.h utils.h

DOCS = 

SCCSCHECK_DIRS =

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
$De_close.o: e_close.c utils.h 
	${CCLINE}e_close.c
$Dis_number.o: is_number.c utils.h 
	${CCLINE}is_number.c
$Dmemxxx.o: memxxx.c ifdefs.h 
	${CCLINE}memxxx.c
$Dquery_user.o: query_user.c utils.h 
	${CCLINE}query_user.c
$Drm_rf.o: rm_rf.c utils.h 
	${CCLINE}rm_rf.c
$Dserverhost.o: serverhost.c serverhost.h 
	${CCLINE}serverhost.c
$Dso.o: so.c ifdefs.h alloc.h so.h strcache.h utils.h 
	${CCLINE}so.c
$Dstrcache.o: strcache.c ifdefs.h strcache.h 
	${CCLINE}strcache.c
$Dregex.o: regex.c regex.h 
	${CCLINE}regex.c
$Dswap_longs.o: swap_longs.c utils.h 
	${CCLINE}swap_longs.c
$Dappltofl.o: appltofl.c ifdefs.h utils.h 
	${CCLINE}appltofl.c
$Dfmtostr.o: fmtostr.c utils.h 
	${CCLINE}fmtostr.c
$Dfttostr.o: fttostr.c utils.h 
	${CCLINE}fttostr.c
$Dfopnew.o: fopnew.c utils.h 
	${CCLINE}fopnew.c
$Dstrtobsz.o: strtobsz.c utils.h 
	${CCLINE}strtobsz.c
$Dmtrprog_vers.o: mtrprog_vers.c utils.h sccsdata.h 
	${CCLINE}mtrprog_vers.c
$Dsccsdata.o: sccsdata.c sccsdata.h 
	${CCLINE}sccsdata.c
