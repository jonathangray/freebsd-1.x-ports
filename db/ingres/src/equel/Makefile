# Generated automatically from Makefile.in by configure.
#
#  Makefile for INGRES EQUEL precompiler
#
#	@(#)Makefile	8.3	1/28/87
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib
BIN=	$(INGRES)/bin
SYSBIN=	/usr/local

OBJS= 	main.o yylex.o grammar.o cmap.o cvar.o include.o name.o getkey.o \
	number.o operator.o prtout.o string.o comment.o retrieve.o display.o \
	getch.o yyerror.o nalloc.o 
HDRS=	constants.h globals.h
SRCS= 	main.c yylex.c grammar.y cmap.c cvar.c include.c name.c getkey.c \
	number.c operator.c prtout.c string.c comment.c retrieve.c display.c \
	getch.c yyerror.c nalloc.c tokens.y

GUTIL=	$(LIB)/libgutil.a
LIBS=	$(GUTIL) -lm

LINTFLAGS= -huc -I$H
CC= gcc
RANLIB= ranlib
OPTIM= -O
MOAN= -Wall
DEFS=  -DHAVE_STRING_H=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 -DHAVE_SGTTY_H=1 -DHAVE_FCNTL_H=1 -DHAVE_MACHINE_ENDIAN_H=1 -DHAVE_SYS_WAIT_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_RESOURCE_H=1 -DTIME_WITH_SYS_TIME=1 -DRETSIGTYPE=void -DDIRENT=1 -DDECLARED_SYS_NERR=1 -DDECLARED_SYS_ERRLIST=1 -DHAVE_STRERROR=1 -DHAVE_STRCSPN=1
CFLAGS= -DINGRES_EQUEL -I$(H) $(OPTIM) $(DEFS) $(MOAN)
LDFLAGS= $(OPTIM)
YFLAGS=	-d
GET=	sccs get
REL=
PR=	pr
LINT=	lint
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Equel Precompiler
CP=	cp
CHMOD=	chmod
CHOWN=	-chown
BINMODE=755

equel: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o equel
	$(CHMOD) $(BINMODE) equel
	ls -l equel; size equel

new:
	rm -f equel

clean:
	rm -f *.o
	rm -f a.out y.tab.c y.tab.h core equel grammar.c

install: $(BIN)/equel

$(BIN)/equel: equel
	$(CP) equel $(BIN)/equel

sysinstall: equel
	$(CP) equel $(SYSBIN)/equel
	$(CHOWN) ingres $(SYSBIN)/equel

lint:
	$(LINT) $(LINTFLAGS) $(SRCS)

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 8)" -n Makefile 
	$(VGRIND) -h "$(HEADER) (Release 8)" *.[hm] *.[csy]
	sed /SCCSID/d < index > tmp
	mv tmp index
	$(VGRIND) -h "$(HEADER) (Release 8)" -x index

tags: sources
	-fgrep ../ tags > othertags
	ctags  $(SRCS) $(HDRS)
	sort -o tags tags othertags
	-rm -f othertags

#
# DANGER	DANGER	DANGER	DANGER	DANGER	DANGER
# The following two entries should NOT be executed by the user.
# These entries are only for the Makefile in conf to construct
# the system tag files.
#
systags: 
	ctags $(SRCS) $(HDRS)

#
# We assume that systags has already been done.
#
installtags:
	sed "/	equel/d" ../tags | sed "s/	/	..\//" > othertags
	sort -o tags tags othertags
	-rm -f othertags systags

sources: $(SRCS) $(HDRS)

$(HDRS):
	$(GET) $(REL) $@
$(SRCS):
	$(GET) $(REL) $@

y.tab.h: grammar.c

depend:
	grep '^#[ 	]*include' $(SRCS) | sed -n -f ../h/depend.sed > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

main.o:$H/ingres.h
main.o:constants.h
main.o:globals.h
main.o:$H/sccs.h
main.o:$H/protos.h
yylex.o:constants.h
yylex.o:globals.h
yylex.o:y.tab.h
yylex.o:$H/sccs.h
yylex.o:$H/protos.h
grammar.o:constants.h
grammar.o:globals.h
grammar.o:$H/sccs.h
grammar.o:$H/protos.h
grammar.o:tokens.y
cmap.o:constants.h
cmap.o:$H/sccs.h
cmap.o:$H/protos.h
cvar.o:constants.h
cvar.o:globals.h
cvar.o:$H/sccs.h
cvar.o:$H/protos.h
include.o:constants.h
include.o:globals.h
include.o:$H/sccs.h
include.o:$H/protos.h
name.o:constants.h
name.o:globals.h
name.o:y.tab.h
name.o:$H/sccs.h
name.o:$H/protos.h
getkey.o:constants.h
getkey.o:globals.h
getkey.o:$H/sccs.h
getkey.o:$H/protos.h
number.o:constants.h
number.o:globals.h
number.o:y.tab.h
number.o:$H/sccs.h
number.o:$H/protos.h
operator.o:constants.h
operator.o:globals.h
operator.o:y.tab.h
operator.o:$H/sccs.h
operator.o:$H/protos.h
prtout.o:constants.h
prtout.o:globals.h
prtout.o:$H/sccs.h
prtout.o:$H/protos.h
string.o:constants.h
string.o:globals.h
string.o:y.tab.h
string.o:$H/sccs.h
string.o:$H/protos.h
comment.o:constants.h
comment.o:globals.h
comment.o:$H/sccs.h
comment.o:$H/protos.h
retrieve.o:constants.h
retrieve.o:globals.h
retrieve.o:$H/sccs.h
retrieve.o:$H/protos.h
display.o:constants.h
display.o:globals.h
display.o:$H/sccs.h
display.o:$H/protos.h
getch.o:constants.h
getch.o:globals.h
getch.o:$H/sccs.h
getch.o:$H/protos.h
yyerror.o:constants.h
yyerror.o:globals.h
yyerror.o:y.tab.h
yyerror.o:$H/sccs.h
yyerror.o:$H/protos.h
nalloc.o:$H/sccs.h
nalloc.o:$H/protos.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
