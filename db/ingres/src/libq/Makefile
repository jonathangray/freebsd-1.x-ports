# Generated automatically from Makefile.in by configure.
#
#  Makefile for INGRES EQUEL runtime library
#
#	@(#)Makefile	8.3	12/18/85
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

OBJS=	IIw_left.o IIw_right.o IIcvar.o IIgettup.o IIn_get.o IIn_ret.o \
	IIingres.o IIretrieve.o IIsetup.o IIflushtup.o IIsync.o \
	IIwrite.o IIexit.o IIsyserr.o IIconvert.o IIerror.o IIp_err.o\
	IIgetc.o IIresync.o IIreadinp.o IIpb_get.o IIpb_put.o IIpb_read.o \
	IIpb_rphys.o IIpb_flush.o IIpb_write.o IIpb_wphys.o IIpb_prime.o \
	IIitos.o  IIconcatv.o IIbmove.o IIlength.o IIatoi.o IIsequal.o
SRCS1=	IIw_left.c IIw_right.c IIcvar.c IIgettup.c IIn_get.c IIn_ret.c \
	IIingres.c IIretrieve.c IIsetup.c IIflushtup.c IIsync.c \
	IIwrite.c IIexit.c IIsyserr.c IIconvert.c IIerror.c IIp_err.c\
	IIgetc.c IIresync.c IIreadinp.c IIpb_get.c IIpb_put.c IIpb_read.c \
	IIpb_rphys.c IIpb_flush.c IIpb_write.c IIpb_wphys.c IIpb_prime.c \
	IIitos.c  IIconcatv.c IIbmove.c IIlength.c IIatoi.c IIsequal.c
HDRS=	IIglobals.h
SRCS=	$(SRCS1) 

SYSLIB=	/usr/local/lib

CC= gcc
RANLIB= ranlib
DEFS=  -DHAVE_STRING_H=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 -DHAVE_SGTTY_H=1 -DHAVE_FCNTL_H=1 -DHAVE_MACHINE_ENDIAN_H=1 -DHAVE_SYS_WAIT_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_RESOURCE_H=1 -DTIME_WITH_SYS_TIME=1 -DRETSIGTYPE=void -DDIRENT=1 -DDECLARED_SYS_NERR=1 -DDECLARED_SYS_ERRLIST=1 -DHAVE_STRERROR=1 -DHAVE_STRCSPN=1
OPTIM= -O
MOAN= -Wall
LDFLAGS= $(OPTIM)
CFLAGS= -DINGRES_LIBQ -I$(H) -I../ctlmod $(OPTIM) $(DEFS) $(MOAN)
LINTFLAGS= -huc -I$H -I../ctlmod
AR=	-ar
ARFLAGS=rvu
CP=	cp
CHMOD=	chmod
CHOWN=	-chown
LIBMODE=664
GET=	sccs get
REL=
LINT=	lint
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	'Equel Runtime Library'

libq.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(CHMOD) $(LIBMODE) libq.a
	$(RANLIB) libq.a

install: $(LIB)/libq.a

$(LIB)/libq.a: libq.a
	$(CP) libq.a $(LIB)/libq.a
	$(RANLIB) $(LIB)/libq.a

sysinstall: $(SYSLIB)/libq.a

$(SYSLIB)/libq.a: libq.a
	$(CP) libq.a $(SYSLIB)/libq.a
	$(CHOWN) ingres $(SYSLIB)/libq.a
	$(RANLIB) $(SYSLIB)/libq.a

sources: $(SRCS) $(HDRS)

$(SRCS):
	$(GET) $(REL) $@

new:
	rm -f libq.a

clean:
	rm -f *.o
	rm -f a.out core libq.a

lint:
	$(LINT) $(LINTFLAGS) $(SRCS1)

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
	sed "s/	/	libq\//" tags > systags
	cat systags >> ../tags

#
# We assume that systags has already been done.
#
installtags:
	sed "/	libq/d" ../tags | sed "s/	/	..\//" > othertags
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

IIw_left.o:$H/ingres.h
IIw_left.o:../equel/constants.h
IIw_left.o:IIglobals.h
IIw_left.o:$H/sccs.h
IIw_left.o:$H/errors.h
IIw_left.o:$H/protos.h
IIw_right.o:$H/ingres.h
IIw_right.o:../equel/constants.h
IIw_right.o:IIglobals.h
IIw_right.o:$H/sccs.h
IIw_right.o:$H/errors.h
IIw_right.o:$H/protos.h
IIcvar.o:$H/ingres.h
IIcvar.o:$H/symbol.h
IIcvar.o:IIglobals.h
IIcvar.o:$H/sccs.h
IIcvar.o:$H/protos.h
IIgettup.o:$H/ingres.h
IIgettup.o:$H/symbol.h
IIgettup.o:IIglobals.h
IIgettup.o:$H/sccs.h
IIgettup.o:$H/errors.h
IIgettup.o:$H/protos.h
IIn_get.o:$H/ingres.h
IIn_get.o:$H/symbol.h
IIn_get.o:IIglobals.h
IIn_get.o:$H/sccs.h
IIn_get.o:$H/protos.h
IIn_ret.o:$H/ingres.h
IIn_ret.o:$H/symbol.h
IIn_ret.o:IIglobals.h
IIn_ret.o:$H/sccs.h
IIn_ret.o:$H/errors.h
IIn_ret.o:$H/protos.h
IIingres.o:$H/ingres.h
IIingres.o:$H/symbol.h
IIingres.o:$H/aux.h
IIingres.o:IIglobals.h
IIingres.o:$H/sccs.h
IIingres.o:$H/protos.h
IIretrieve.o:$H/ingres.h
IIretrieve.o:$H/symbol.h
IIretrieve.o:IIglobals.h
IIretrieve.o:$H/sccs.h
IIretrieve.o:$H/protos.h
IIsetup.o:$H/ingres.h
IIsetup.o:$H/symbol.h
IIsetup.o:IIglobals.h
IIsetup.o:$H/sccs.h
IIsetup.o:$H/protos.h
IIflushtup.o:$H/ingres.h
IIflushtup.o:$H/aux.h
IIflushtup.o:$H/symbol.h
IIflushtup.o:IIglobals.h
IIflushtup.o:$H/sccs.h
IIflushtup.o:$H/protos.h
IIsync.o:$H/ingres.h
IIsync.o:$H/symbol.h
IIsync.o:IIglobals.h
IIsync.o:$H/sccs.h
IIsync.o:$H/protos.h
IIwrite.o:$H/ingres.h
IIwrite.o:$H/symbol.h
IIwrite.o:IIglobals.h
IIwrite.o:$H/sccs.h
IIwrite.o:$H/protos.h
IIexit.o:$H/ingres.h
IIexit.o:$H/symbol.h
IIexit.o:IIglobals.h
IIexit.o:$H/sccs.h
IIexit.o:$H/protos.h
IIsyserr.o:$H/sccs.h
IIsyserr.o:$H/protos.h
IIconvert.o:$H/ingres.h
IIconvert.o:$H/symbol.h
IIconvert.o:$H/sccs.h
IIconvert.o:$H/protos.h
IIerror.o:$H/ingres.h
IIerror.o:$H/symbol.h
IIerror.o:IIglobals.h
IIerror.o:$H/sccs.h
IIerror.o:$H/protos.h
IIp_err.o:$H/ingres.h
IIp_err.o:$H/aux.h
IIp_err.o:$H/version.h
IIp_err.o:IIglobals.h
IIp_err.o:$H/sccs.h
IIp_err.o:$H/protos.h
IIgetc.o:$H/ingres.h
IIgetc.o:IIglobals.h
IIgetc.o:$H/sccs.h
IIgetc.o:$H/protos.h
IIresync.o:$H/ingres.h
IIresync.o:IIglobals.h
IIresync.o:$H/sccs.h
IIresync.o:$H/protos.h
IIreadinp.o:$H/ingres.h
IIreadinp.o:$H/aux.h
IIreadinp.o:IIglobals.h
IIreadinp.o:$H/sccs.h
IIreadinp.o:$H/errors.h
IIreadinp.o:$H/protos.h
IIpb_get.o:$H/useful.h
IIpb_get.o:$H/sccs.h
IIpb_get.o:$H/protos.h
IIpb_put.o:$H/useful.h
IIpb_put.o:$H/sccs.h
IIpb_put.o:$H/protos.h
IIpb_read.o:$H/ingres.h
IIpb_read.o:IIglobals.h
IIpb_read.o:$H/sccs.h
IIpb_read.o:$H/protos.h
IIpb_rphys.o:$H/sccs.h
IIpb_rphys.o:$H/protos.h
IIpb_flush.o:$H/sccs.h
IIpb_flush.o:$H/protos.h
IIpb_write.o:$H/ingres.h
IIpb_write.o:IIglobals.h
IIpb_write.o:$H/sccs.h
IIpb_write.o:$H/protos.h
IIpb_wphys.o:$H/sccs.h
IIpb_wphys.o:$H/protos.h
IIpb_prime.o:$H/symbol.h
IIpb_prime.o:$H/sccs.h
IIpb_prime.o:$H/protos.h
IIitos.o:$H/sccs.h
IIitos.o:$H/protos.h
IIconcatv.o:$H/sccs.h
IIconcatv.o:$H/protos.h
IIbmove.o:$H/sccs.h
IIbmove.o:$H/protos.h
IIlength.o:$H/ingres.h
IIlength.o:$H/aux.h
IIlength.o:$H/sccs.h
IIlength.o:$H/protos.h
IIatoi.o:$H/sccs.h
IIatoi.o:$H/protos.h
IIsequal.o:$H/sccs.h
IIsequal.o:$H/protos.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
