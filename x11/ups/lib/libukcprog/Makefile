#  @(#)Makefile	1.12 23/6/92 UKC

CFLAGS = -I$Sukcheaders ${CCFLAGS} ${MORE_CFLAGS}

D = ${OBJDIR}${SLASH}
S = ${SRCDIR}${SLASH}
CCLINE = ${IN_OBJDIR} ${CC} ${CFLAGS} -c $S

#  Install directories for include files and the library itself.
INCLUDE = /usr/include/local
LIB = /usr/local/lib
MAN = /usr/local/man/man3

SCCS = sccs
CHECKSCCS = ${SCCS} check

OBJS =	$Dalloc.o $De_malloc.o $De_realloc.o $Derrf.o \
	$Dformf.o $Dpanic.o $Dssplit.o $Dstrf.o $Dstrnf.o $Dstrsave.o \
	$Dfpgetline.o $Dstrtol.o $Dconfig.o $Dip.o \
	$Dsccsdata.o

SRCS =	alloc.c e_malloc.c e_realloc.c errf.c \
	formf.c panic.c ssplit.c strf.c strnf.c strsave.c \
	fpgetline.c strtol.c config.c ip.c

HDRS = ukcprog.h ukcheaders/ukcstdlib.h ukcheaders/ukcstring.h

DOCS = doc/ukcprog.3

FILES = ${HDRS} ${SRCS} ${DOCS} Makefile

all: $Dlibukcprog.a

sccsfile:
	@sccsinfo ${FILES}
	@checklist
	@newer CHANGES ${FILES}
	@${SCCS} edit sccsdata.c
	@mksccs -changefile CHANGES -versfunc ukcprog.h ukcprog ${FILES}
	@${SCCS} delget sccsdata.c

install: checksccs ${LIB}/libukcprog.a ${INCLUDE}/ukcprog.h ${MAN}/ukcprog.3

${LIB}/libukcprog.a: $Dlibukcprog.a
	install -c -m 644 $Dlibukcprog.a ${LIB}/libukcprog.a
	cd ${LIB} && ranlib libukcprog.a

${INCLUDE}/ukcprog.h: ukcprog.h
	install -c -m 644 $? $@

${MAN}/ukcprog.3: doc/ukcprog.3
	install -c -m 644 $? $@

$Dlibukcprog.a: $D/. ${OBJS}
	rm -f $@
	ar cq $@ ${OBJS}
	${IN_OBJDIR} ranlib libukcprog.a

checksccs:
	@${CHECKSCCS}

list: ${FILES}
	@echo ${FILES} sccsdata.c | tr ' ' '\012'

clean:
	rm -f ${OBJS} $Dlibukcprog.a

deps:
	@deps -Iukcheaders ${SRCS} sccsdata.c


#  Make subdirectories for object code files
#  Use $D/. so we don't get a null target when $D is undefined.
#  Use sh -c to avoid exit status problems from the || construct.
$D/.:
	-@(IFS=/; for i in $D; do test -d $$i || mkdir $$i; cd $$i; done)

$Dalloc.o: alloc.c ukcprog.h 
	${CCLINE}alloc.c
$De_malloc.o: e_malloc.c ukcprog.h 
	${CCLINE}e_malloc.c
$De_realloc.o: e_realloc.c ukcprog.h 
	${CCLINE}e_realloc.c
$Derrf.o: errf.c ukcprog.h 
	${CCLINE}errf.c
$Dformf.o: formf.c ukcprog.h 
	${CCLINE}formf.c
$Dpanic.o: panic.c ukcprog.h 
	${CCLINE}panic.c
$Dssplit.o: ssplit.c ukcprog.h 
	${CCLINE}ssplit.c
$Dstrf.o: strf.c ukcprog.h 
	${CCLINE}strf.c
$Dstrnf.o: strnf.c ukcprog.h 
	${CCLINE}strnf.c
$Dstrsave.o: strsave.c ukcprog.h 
	${CCLINE}strsave.c
$Dfpgetline.o: fpgetline.c ukcprog.h 
	${CCLINE}fpgetline.c
$Dstrtol.o: strtol.c 
	${CCLINE}strtol.c
$Dconfig.o: config.c ukcprog.h 
	${CCLINE}config.c
$Dip.o: ip.c ukcprog.h 
	${CCLINE}ip.c
$Dsccsdata.o: sccsdata.c ukcprog.h 
	${CCLINE}sccsdata.c
