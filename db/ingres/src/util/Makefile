# Generated automatically from Makefile.in by configure.
#
#  Makefile for INGRES util library
#
#	@(#)Makefile	8.4	6/12/88
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

OBJS1=	acc_addbuf.o add_prim.o batch.o batchxtra.o cleanrel.o \
	closecat.o closer.o delete.o do_u_flag.o \
	errfilen.o formatpg.o getnuser.o gettree.o getuser.o \
	initucode.o dblocks.o inpcloser.o insert.o markopen.o \
	noclose.o null_fn.o opencat.o cat_desc.o openr.o \
	getequal.o get.o printdesc.o \
	replace.o findbest.o add_ovflo.o cslocks.o \
	find.o last_page.o ndxsearch.o compare.o newlino.o \
	paramd.o out_arg.o rhash.o \
	rllocks.o rub.o scan_dups.o setkey.o \
	trim_relna.o tuple.o writebatch.o accbuf.o page.o \
	pglocks.o globals.o readadmin.o checkadmin.o lockreq.o \
	ingresname.o utility.o lock.o \
	btree.o get_tid.o \
	sort.o
OBJS2=	append.o atof.o bequal.o bitcnt.o bitpos.o cat.o \
	concat.o freebuf.o ftoa.o fullwait.o \
	getufield.o iocv.o itoa.o length.o \
	xalloc.o need.o oatoi.o pad.o pmove.o \
	prargs.o scompare.o sequal.o set_si_buf.o set_so_buf.o \
	setflag.o trace.o atol.o smove.o \
	lprintf.o clrmem.o xputchar.o ztack.o syserr.o \
	bmove.o perror.o locv.o capital.o extern.o sre.o

SRCS1=	acc_addbuf.c add_prim.c batch.c batchxtra.c cleanrel.c \
	closecat.c closer.c delete.c do_u_flag.c \
	errfilen.c formatpg.c getnuser.c gettree.c getuser.c \
	initucode.c dblocks.c inpcloser.c insert.c markopen.c \
	noclose.c null_fn.c opencat.c cat_desc.c openr.c \
	getequal.c get.c printdesc.c \
	btree.c get_tid.c
SRCS2=	replace.c findbest.c add_ovflo.c cslocks.c \
	find.c last_page.c ndxsearch.c compare.c newlino.c \
	paramd.c out_arg.c rhash.c \
	rllocks.c rub.c scan_dups.c setkey.c \
	trim_relna.c tuple.c writebatch.c accbuf.c page.c \
	pglocks.c globals.c readadmin.c checkadmin.c lockreq.c \
	ingresname.c utility.c lock.c \
	sort.c
SRCS3=	append.c atof.c bequal.c bitcnt.c bitpos.c cat.c \
	concat.c freebuf.c ftoa.c fullwait.c \
	getufield.c iocv.c itoa.c length.c \
	xalloc.c need.c oatoi.c pad.c pmove.c capital.c extern.c
SRCS4=	prargs.c scompare.c sequal.c set_si_buf.c set_so_buf.c \
	setflag.c trace.c atol.c smove.c \
	xputchar.c ztack.c syserr.c \
	lprintf.c clrmem.c perror.c locv.c sre.c
ASSRC=  bmove.c
SRCS=	$(SRCS1) $(SRCS2) $(SRCS3) $(SRCS4) $(ASSRC)
OBJS=	$(OBJS1) $(OBJS2)

LINTFLAGS = -huc -I$H
AR=	-ar
ARFLAGS=urv
CC= gcc
RANLIB=	ranlib
DEFS=  -DHAVE_STRING_H=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 -DHAVE_SGTTY_H=1 -DHAVE_FCNTL_H=1 -DHAVE_MACHINE_ENDIAN_H=1 -DHAVE_SYS_WAIT_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_RESOURCE_H=1 -DTIME_WITH_SYS_TIME=1 -DRETSIGTYPE=void -DDIRENT=1 -DDECLARED_SYS_NERR=1 -DDECLARED_SYS_ERRLIST=1 -DHAVE_STRERROR=1 -DHAVE_STRCSPN=1
OPTIM= -O
MOAN= -Wall
LDFLAGS= $(OPTIM)
CFLAGS= -DINGRES_IUTIL -DINGRES_GUTIL -I$(H) $(OPTIM) $(DEFS) $(MOAN)
GET=	sccs get
REL=
LINT = lint
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Iutil Library
CP=	cp
CHMOD=	chmod
LIBMODE=664

libutil.a: $(OBJS)
	$(AR) $(ARFLAGS) libutil.a $(OBJS)
	$(CHMOD) $(LIBMODE) libutil.a
	$(RANLIB) libutil.a

install: $(LIB)/libutil.a

$(LIB)/libutil.a: libutil.a
	$(CP) libutil.a $(LIB)/libutil.a
	$(RANLIB) $(LIB)/libutil.a

new:
	rm -f libutil.a

clean:
	rm -f *.o
	rm -f libutil.a a.out core

lint:
	$(LINT) $(LINTFLAGS) $(SRCS1) $(SRCS2)

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 8)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 8)" *.[csy]
	$(VGRIND) -h "$(HEADER) (Release 8)" -x index

sources: $(SRCS)

$(SRCS1):
	$(GET) $(REL) $@
$(SRCS2):
	$(GET) $(REL) $@

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
	sed "s/	/	libutil.a\//" tags > systags
	cat systags >> ../tags

#
# We assume that systags has already been done.
#
installtags:
	sed "/	libutil.a/d" ../tags | sed "s/	/	..\//" > othertags
	sort -o tags tags othertags
	-rm -f othertags systags

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

acc_addbuf.o:$H/ingres.h
acc_addbuf.o:$H/access.h
acc_addbuf.o:$H/sccs.h
acc_addbuf.o:$H/protos.h
add_prim.o:$H/ingres.h
add_prim.o:$H/access.h
add_prim.o:$H/sccs.h
add_prim.o:$H/protos.h
batch.o:$H/ingres.h
batch.o:$H/aux.h
batch.o:$H/catalog.h
batch.o:$H/symbol.h
batch.o:$H/access.h
batch.o:$H/batch.h
batch.o:$H/btree.h
batch.o:$H/sccs.h
batch.o:$H/protos.h
batchxtra.o:$H/ingres.h
batchxtra.o:$H/aux.h
batchxtra.o:$H/symbol.h
batchxtra.o:$H/access.h
batchxtra.o:$H/batch.h
batchxtra.o:$H/sccs.h
batchxtra.o:$H/protos.h
cleanrel.o:$H/ingres.h
cleanrel.o:$H/aux.h
cleanrel.o:$H/access.h
cleanrel.o:$H/sccs.h
cleanrel.o:$H/protos.h
closecat.o:$H/ingres.h
closecat.o:$H/aux.h
closecat.o:$H/access.h
closecat.o:$H/sccs.h
closecat.o:$H/protos.h
closer.o:$H/ingres.h
closer.o:$H/access.h
closer.o:$H/catalog.h
closer.o:$H/sccs.h
closer.o:$H/protos.h
delete.o:$H/ingres.h
delete.o:$H/access.h
delete.o:$H/catalog.h
delete.o:$H/batch.h
delete.o:$H/btree.h
delete.o:$H/sccs.h
delete.o:$H/protos.h
do_u_flag.o:$H/ingres.h
do_u_flag.o:$H/aux.h
do_u_flag.o:$H/sccs.h
do_u_flag.o:$H/protos.h
errfilen.o:$H/ingres.h
errfilen.o:$H/aux.h
errfilen.o:$H/version.h
errfilen.o:$H/sccs.h
errfilen.o:$H/protos.h
formatpg.o:$H/ingres.h
formatpg.o:$H/access.h
formatpg.o:$H/sccs.h
formatpg.o:$H/protos.h
getnuser.o:$H/ingres.h
getnuser.o:$H/aux.h
getnuser.o:$H/sccs.h
gettree.o:$H/ingres.h
gettree.o:$H/catalog.h
gettree.o:$H/tree.h
gettree.o:$H/symbol.h
gettree.o:$H/pv.h
gettree.o:$H/sccs.h
gettree.o:$H/protos.h
getuser.o:$H/ingres.h
getuser.o:$H/aux.h
getuser.o:$H/sccs.h
initucode.o:$H/ingres.h
initucode.o:$H/aux.h
initucode.o:$H/version.h
initucode.o:$H/access.h
initucode.o:$H/lock.h
initucode.o:$H/sccs.h
initucode.o:$H/protos.h
dblocks.o:$H/ingres.h
dblocks.o:$H/lock.h
dblocks.o:$H/sccs.h
dblocks.o:$H/protos.h
inpcloser.o:$H/ingres.h
inpcloser.o:$H/access.h
inpcloser.o:$H/sccs.h
inpcloser.o:$H/protos.h
insert.o:$H/ingres.h
insert.o:$H/access.h
insert.o:$H/catalog.h
insert.o:$H/btree.h
insert.o:$H/sccs.h
insert.o:$H/protos.h
markopen.o:$H/useful.h
markopen.o:$H/sccs.h
markopen.o:$H/protos.h
noclose.o:$H/ingres.h
noclose.o:$H/aux.h
noclose.o:$H/access.h
noclose.o:$H/sccs.h
noclose.o:$H/protos.h
null_fn.o:$H/sccs.h
opencat.o:$H/ingres.h
opencat.o:$H/aux.h
opencat.o:$H/access.h
opencat.o:$H/sccs.h
opencat.o:$H/protos.h
cat_desc.o:$H/ingres.h
cat_desc.o:$H/aux.h
cat_desc.o:$H/access.h
cat_desc.o:$H/sccs.h
openr.o:$H/ingres.h
openr.o:$H/aux.h
openr.o:$H/access.h
openr.o:$H/symbol.h
openr.o:$H/catalog.h
openr.o:$H/sccs.h
openr.o:$H/protos.h
getequal.o:$H/ingres.h
getequal.o:$H/access.h
getequal.o:$H/sccs.h
getequal.o:$H/protos.h
get.o:$H/ingres.h
get.o:$H/aux.h
get.o:$H/access.h
get.o:$H/sccs.h
get.o:$H/protos.h
printdesc.o:$H/ingres.h
printdesc.o:$H/sccs.h
printdesc.o:$H/protos.h
btree.o:$H/ingres.h
btree.o:$H/catalog.h
btree.o:$H/aux.h
btree.o:$H/btree.h
btree.o:$H/batch.h
btree.o:$H/symbol.h
btree.o:$H/access.h
btree.o:$H/sccs.h
btree.o:$H/protos.h
get_tid.o:$H/btree.h
get_tid.o:$H/sccs.h
get_tid.o:$H/protos.h
replace.o:$H/ingres.h
replace.o:$H/access.h
replace.o:$H/catalog.h
replace.o:$H/batch.h
replace.o:$H/btree.h
replace.o:$H/sccs.h
replace.o:$H/protos.h
findbest.o:$H/ingres.h
findbest.o:$H/aux.h
findbest.o:$H/access.h
findbest.o:$H/sccs.h
findbest.o:$H/protos.h
add_ovflo.o:$H/ingres.h
add_ovflo.o:$H/access.h
add_ovflo.o:$H/aux.h
add_ovflo.o:$H/lock.h
add_ovflo.o:$H/sccs.h
add_ovflo.o:$H/protos.h
cslocks.o:$H/ingres.h
cslocks.o:$H/access.h
cslocks.o:$H/lock.h
cslocks.o:$H/sccs.h
cslocks.o:$H/protos.h
find.o:$H/ingres.h
find.o:$H/aux.h
find.o:$H/symbol.h
find.o:$H/access.h
find.o:$H/lock.h
find.o:$H/btree.h
find.o:$H/sccs.h
find.o:$H/protos.h
last_page.o:$H/ingres.h
last_page.o:$H/access.h
last_page.o:$H/sccs.h
last_page.o:$H/protos.h
ndxsearch.o:$H/ingres.h
ndxsearch.o:$H/aux.h
ndxsearch.o:$H/symbol.h
ndxsearch.o:$H/access.h
ndxsearch.o:$H/lock.h
ndxsearch.o:$H/sccs.h
ndxsearch.o:$H/protos.h
compare.o:$H/ingres.h
compare.o:$H/access.h
compare.o:$H/symbol.h
compare.o:$H/aux.h
compare.o:$H/sccs.h
compare.o:$H/protos.h
newlino.o:$H/ingres.h
newlino.o:$H/access.h
newlino.o:$H/sccs.h
newlino.o:$H/protos.h
paramd.o:$H/ingres.h
paramd.o:$H/aux.h
paramd.o:$H/catalog.h
paramd.o:$H/access.h
paramd.o:$H/sccs.h
paramd.o:$H/protos.h
out_arg.o:$H/ingres.h
out_arg.o:$H/aux.h
out_arg.o:$H/sccs.h
out_arg.o:$H/protos.h
rhash.o:$H/ingres.h
rhash.o:$H/aux.h
rhash.o:$H/symbol.h
rhash.o:$H/access.h
rhash.o:$H/lock.h
rhash.o:$H/sccs.h
rhash.o:$H/protos.h
rllocks.o:$H/ingres.h
rllocks.o:$H/lock.h
rllocks.o:$H/sccs.h
rllocks.o:$H/protos.h
rub.o:$H/useful.h
rub.o:$H/sccs.h
rub.o:$H/protos.h
scan_dups.o:$H/ingres.h
scan_dups.o:$H/symbol.h
scan_dups.o:$H/access.h
scan_dups.o:$H/lock.h
scan_dups.o:$H/range.h
scan_dups.o:$H/sccs.h
scan_dups.o:$H/protos.h
setkey.o:$H/ingres.h
setkey.o:$H/symbol.h
setkey.o:$H/sccs.h
setkey.o:$H/protos.h
trim_relna.o:$H/ingres.h
trim_relna.o:$H/sccs.h
tuple.o:$H/ingres.h
tuple.o:$H/symbol.h
tuple.o:$H/access.h
tuple.o:$H/aux.h
tuple.o:$H/catalog.h
tuple.o:$H/btree.h
tuple.o:$H/sccs.h
tuple.o:$H/protos.h
writebatch.o:$H/ingres.h
writebatch.o:$H/aux.h
writebatch.o:$H/symbol.h
writebatch.o:$H/access.h
writebatch.o:$H/batch.h
writebatch.o:$H/sccs.h
writebatch.o:$H/protos.h
accbuf.o:$H/ingres.h
accbuf.o:$H/access.h
accbuf.o:$H/aux.h
accbuf.o:$H/lock.h
accbuf.o:$H/sccs.h
accbuf.o:$H/protos.h
page.o:$H/ingres.h
page.o:$H/access.h
page.o:$H/aux.h
page.o:$H/lock.h
page.o:$H/sccs.h
page.o:$H/protos.h
pglocks.o:$H/ingres.h
pglocks.o:$H/access.h
pglocks.o:$H/lock.h
pglocks.o:$H/sccs.h
pglocks.o:$H/protos.h
globals.o:$H/sccs.h
readadmin.o:$H/ingres.h
readadmin.o:$H/access.h
readadmin.o:$H/aux.h
readadmin.o:$H/lock.h
readadmin.o:$H/sccs.h
readadmin.o:$H/protos.h
checkadmin.o:$H/ingres.h
checkadmin.o:$H/version.h
checkadmin.o:$H/access.h
checkadmin.o:$H/sccs.h
checkadmin.o:$H/protos.h
lockreq.o:$H/ingres.h
lockreq.o:$H/ildr.h
lockreq.o:$H/lock.h
lockreq.o:$H/sccs.h
lockreq.o:$H/protos.h
ingresname.o:$H/ingres.h
ingresname.o:$H/sccs.h
ingresname.o:$H/protos.h
utility.o:$H/endian.h
utility.o:$H/ingres.h
utility.o:$H/access.h
utility.o:$H/sccs.h
utility.o:$H/protos.h
lock.o:$H/sccs.h
lock.o:$H/ingres.h
lock.o:$H/protos.h
sort.o:$H/ingres.h
sort.o:$H/batch.h
sort.o:$H/version.h
sort.o:$H/pv.h
sort.o:$H/symbol.h
sort.o:$H/access.h
sort.o:$H/sccs.h
sort.o:$H/protos.h
append.o:$H/sccs.h
atof.o:$H/sccs.h
bequal.o:$H/useful.h
bequal.o:$H/sccs.h
bitcnt.o:$H/sccs.h
bitpos.o:$H/sccs.h
cat.o:$H/sccs.h
cat.o:$H/useful.h
concat.o:$H/sccs.h
freebuf.o:$H/sccs.h
freebuf.o:nodbuf.h
freebuf.o:$H/protos.h
ftoa.o:$H/sccs.h
ftoa.o:$H/protos.h
fullwait.o:$H/useful.h
fullwait.o:$H/sccs.h
fullwait.o:$H/protos.h
getufield.o:$H/sccs.h
iocv.o:$H/sccs.h
iocv.o:$H/useful.h
iocv.o:$H/protos.h
itoa.o:$H/sccs.h
length.o:$H/sccs.h
xalloc.o:$H/useful.h
xalloc.o:$H/sccs.h
xalloc.o:$H/protos.h
need.o:$H/sccs.h
need.o:nodbuf.h
need.o:$H/protos.h
oatoi.o:$H/sccs.h
pad.o:$H/sccs.h
pad.o:$H/protos.h
pmove.o:$H/sccs.h
capital.o:$H/sccs.h
extern.o:$H/resp.h
extern.o:$H/ingres.h
extern.o:$H/protos.h
prargs.o:$H/sccs.h
prargs.o:$H/protos.h
scompare.o:$H/useful.h
scompare.o:$H/sccs.h
sequal.o:$H/useful.h
sequal.o:$H/sccs.h
sequal.o:$H/protos.h
set_si_buf.o:$H/ingres.h
set_si_buf.o:$H/sccs.h
set_so_buf.o:$H/ingres.h
set_so_buf.o:$H/sccs.h
setflag.o:$H/sccs.h
trace.o:$H/useful.h
trace.o:$H/sccs.h
atol.o:$H/useful.h
atol.o:$H/sccs.h
smove.o:$H/sccs.h
xputchar.o:$H/sccs.h
ztack.o:$H/sccs.h
ztack.o:$H/useful.h
ztack.o:$H/protos.h
syserr.o:$H/sccs.h
syserr.o:$H/protos.h
lprintf.o:$H/useful.h
lprintf.o:$H/sccs.h
lprintf.o:$H/protos.h
clrmem.o:$H/sccs.h
perror.o:$H/sccs.h
locv.o:$H/sccs.h
locv.o:$H/useful.h
sre.o:$H/protos.h
sre.o:$H/sre.h
bmove.o:$H/protos.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
