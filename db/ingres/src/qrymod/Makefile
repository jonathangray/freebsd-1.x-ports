# Generated automatically from Makefile.in by configure.
#
#  Makefile for INGRES query modification module
#
#	@(#)Makefile	8.3	12/18/85
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

CTLMOD=	$(LIB)/libctlmod.a
IUTIL=	$(LIB)/libiutil.a
GUTIL=	$(LIB)/libgutil.a
LIBS=	$(CTLMOD) $(IUTIL) $(GUTIL)

CC= gcc
RANLIB= ranlib
DEFS=  -DHAVE_STRING_H=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 -DHAVE_SGTTY_H=1 -DHAVE_FCNTL_H=1 -DHAVE_MACHINE_ENDIAN_H=1 -DHAVE_SYS_WAIT_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_RESOURCE_H=1 -DTIME_WITH_SYS_TIME=1 -DRETSIGTYPE=void -DDIRENT=1 -DDECLARED_SYS_NERR=1 -DDECLARED_SYS_ERRLIST=1 -DHAVE_STRERROR=1 -DHAVE_STRCSPN=1
OPTIM= -O
MOAN= -Wall
LDFLAGS= $(OPTIM)
CFLAGS= -DINGRES_QRYMOD -I$(H) $(OPTIM) $(DEFS) $(MOAN)
LINTFLAGS= -huc -I$H
AR=	-ar
ARFLAGS=rvu
CP=	cp
CHMOD=	chmod
LIBMODE=664
BINMODE=4740
ROMODE=	444
GET=	sccs get
REL=
LINT=	lint
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Query Modification
CTAGS=	/bin/csh /usr/ucb/ctags

OBJS=	d_integ.o d_prot.o d_view.o puttree.o qrymod.o \
	integrity.o protect.o view.o trscan.o \
	issue.o norml.o tree.o util.o
SRCS=	integrity.c issue.c norml.c protect.c \
	qrymod.c tree.c trscan.c \
	util.c view.c puttree.c \
	d_view.c d_prot.c d_integ.c 
HDRS=	qrymod.h

libqrymod.a: $(OBJS)
	$(AR) $(ARFLAGS) libqrymod.a $(OBJS)
	$(CHMOD) $(LIBMODE) libqrymod.a
	$(RANLIB) libqrymod.a

install: $(LIB)/libqrymod.a

$(LIB)/libqrymod.a: libqrymod.a
	$(CP) libqrymod.a $(LIB)/libqrymod.a
	$(RANLIB) $(LIB)/libqrymod.a

.PRECIOUS: libqrymod.a

sources: $(SRCS) $(HDRS)

$(SRCS):
	$(GET) $(REL) $@

lint:
	$(LINT) $(LINTFLAGS) $(SRCS)

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 8)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 8)" *.[hm] *.[csy]
	sed /SCCSID/d < index > tmp
	mv tmp index
	$(VGRIND) -h "$(HEADER) (Release 8)" -x index

new:
	rm -f libqrymod.a

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
	sed "s/	/	qrymod\//" tags > systags
	cat systags >> ../tags

#
# We assume that systags has already been done.
#
installtags:
	sed "/	qrymod/d" ../tags | sed "s/	/	..\//" > othertags
	sort -o tags tags othertags
	-rm -f othertags systags

clean:
	rm -f *.o
	rm -f core a.out libqrymod.a

$(OBJS): qrymod.h

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

integrity.o:$H/ingres.h
integrity.o:$H/aux.h
integrity.o:$H/catalog.h
integrity.o:$H/access.h
integrity.o:$H/tree.h
integrity.o:$H/symbol.h
integrity.o:qrymod.h
integrity.o:$H/sccs.h
integrity.o:$H/protos.h
issue.o:$H/ingres.h
issue.o:$H/aux.h
issue.o:$H/symbol.h
issue.o:$H/tree.h
issue.o:$H/pv.h
issue.o:qrymod.h
issue.o:$H/sccs.h
issue.o:$H/protos.h
norml.o:$H/ingres.h
norml.o:$H/aux.h
norml.o:$H/tree.h
norml.o:$H/symbol.h
norml.o:$H/sccs.h
norml.o:$H/protos.h
protect.o:$H/ingres.h
protect.o:$H/aux.h
protect.o:$H/catalog.h
protect.o:$H/symbol.h
protect.o:$H/tree.h
protect.o:qrymod.h
protect.o:$H/sccs.h
protect.o:$H/errors.h
protect.o:$H/protos.h
qrymod.o:$H/ingres.h
qrymod.o:$H/aux.h
qrymod.o:$H/pv.h
qrymod.o:$H/func.h
qrymod.o:$H/tree.h
qrymod.o:qrymod.h
qrymod.o:$H/sccs.h
qrymod.o:$H/protos.h
tree.o:$H/ingres.h
tree.o:$H/tree.h
tree.o:$H/symbol.h
tree.o:qrymod.h
tree.o:$H/sccs.h
tree.o:$H/protos.h
trscan.o:$H/ingres.h
trscan.o:$H/symbol.h
trscan.o:$H/tree.h
trscan.o:qrymod.h
trscan.o:$H/sccs.h
trscan.o:$H/errors.h
trscan.o:$H/protos.h
util.o:$H/ingres.h
util.o:$H/aux.h
util.o:$H/symbol.h
util.o:$H/tree.h
util.o:qrymod.h
util.o:$H/sccs.h
util.o:$H/protos.h
view.o:$H/ingres.h
view.o:$H/aux.h
view.o:$H/symbol.h
view.o:$H/tree.h
view.o:qrymod.h
view.o:$H/sccs.h
view.o:$H/errors.h
view.o:$H/protos.h
puttree.o:$H/ingres.h
puttree.o:$H/catalog.h
puttree.o:$H/symbol.h
puttree.o:$H/lock.h
puttree.o:$H/tree.h
puttree.o:qrymod.h
puttree.o:$H/sccs.h
puttree.o:$H/protos.h
d_view.o:$H/ingres.h
d_view.o:$H/aux.h
d_view.o:$H/tree.h
d_view.o:$H/symbol.h
d_view.o:$H/catalog.h
d_view.o:$H/pv.h
d_view.o:$H/func.h
d_view.o:qrymod.h
d_view.o:$H/sccs.h
d_view.o:$H/protos.h
d_prot.o:$H/ingres.h
d_prot.o:$H/aux.h
d_prot.o:$H/catalog.h
d_prot.o:$H/access.h
d_prot.o:$H/tree.h
d_prot.o:$H/symbol.h
d_prot.o:$H/lock.h
d_prot.o:$H/pv.h
d_prot.o:$H/func.h
d_prot.o:qrymod.h
d_prot.o:$H/sccs.h
d_prot.o:$H/errors.h
d_prot.o:$H/protos.h
d_integ.o:$H/ingres.h
d_integ.o:$H/aux.h
d_integ.o:$H/catalog.h
d_integ.o:$H/tree.h
d_integ.o:$H/symbol.h
d_integ.o:$H/pv.h
d_integ.o:$H/resp.h
d_integ.o:$H/func.h
d_integ.o:qrymod.h
d_integ.o:$H/sccs.h
d_integ.o:$H/errors.h
d_integ.o:$H/protos.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
