# Makefile for the arg library.

# @(#)Makefile	1.12 26/4/92 (UKC)

# Optional multi-architecture support
D = ${OBJDIR}${SLASH}
S = ${SRCDIR}${SLASH}

LIBNAME = arg
HDRNAME = arg
LIBDIRNAME = arg
LINTLIBS = 

TESTSRCS = testarg.c
LIBSRCS = arg_brace.c arg_dir.c arg_glob.c arg_lparse.c arg_match.c arg_quote.c arg_redir.c arg_user.c arg_vec.c arg_version.c
OBJS = $Darg_brace.o $Darg_dir.o $Darg_glob.o $Darg_lparse.o $Darg_match.o $Darg_quote.o $Darg_redir.o $Darg_user.o $Darg_vec.o $Darg_version.o $Dsccsdata.o
HDRS = arg.h sccsdata.h 

DOCS = 

LIBSRC = ..
UKCLIB = ${LIBSRC}/libukcprog/$Dlibukcprog.a	# Developement ukcprog library
TESTLIBDEPS = ${UKCLIB}
TESTLIBS = ${UKCLIB}

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

deps: ${SRCS} sccsdata.c
	deps ${DEPS_DEVELHDRS} ${SRCS} sccsdata.c

# End common part of Makefile

# Machine generated .o file dependencies
$Darg_brace.o: arg_brace.c arg.h 
	${CCLINE}arg_brace.c
$Darg_dir.o: arg_dir.c arg.h 
	${CCLINE}arg_dir.c
$Darg_glob.o: arg_glob.c arg.h 
	${CCLINE}arg_glob.c
$Darg_lparse.o: arg_lparse.c arg.h 
	${CCLINE}arg_lparse.c
$Darg_match.o: arg_match.c arg.h 
	${CCLINE}arg_match.c
$Darg_quote.o: arg_quote.c arg.h 
	${CCLINE}arg_quote.c
$Darg_redir.o: arg_redir.c arg.h 
	${CCLINE}arg_redir.c
$Darg_user.o: arg_user.c arg.h 
	${CCLINE}arg_user.c
$Darg_vec.o: arg_vec.c arg.h 
	${CCLINE}arg_vec.c
$Darg_version.o: arg_version.c arg.h sccsdata.h 
	${CCLINE}arg_version.c
$Dtestarg.o: testarg.c arg.h 
	${CCLINE}testarg.c
$Dsccsdata.o: sccsdata.c sccsdata.h 
	${CCLINE}sccsdata.c
