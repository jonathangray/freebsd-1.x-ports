# Generated automatically from Makefile.in by configure.
#
#  Makefile for the INGRES control module
#
#	@(#)Makefile	8.4	4/22/86
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

OBJS=	main.o error.o proc_err.o initp.o setp.o resetp.o \
	init_qt.o call.o sysdump.o mapvars.o \
	do_seq.o readinput.o do_st.o send_off.o call_fn.o \
	readmon.o readqry.o writeqry.o range.o treepr.o \
	pb_prime.o pb_get.o pb_tput.o pb_put.o pb_flush.o \
	pb_read.o pb_write.o pb_rphys.o pb_wphys.o pb_dump.o \
	cm_reset.o cm_cleanup.o cm_close.o \
	prvect.o getp.o monitor.o av_files.o
HDRS=	pipes.h proc.h state.h ctlmod.h 
SRCS1=	main.c error.c proc_err.c initp.c setp.c resetp.c \
	init_qt.c call.c sysdump.c mapvars.c \
	do_seq.c readinput.c do_st.c send_off.c call_fn.c \
	readmon.c readqry.c writeqry.c range.c treepr.c
SRCS2=	pb_prime.c pb_get.c pb_tput.c pb_put.c pb_flush.c \
	pb_read.c pb_write.c pb_rphys.c pb_wphys.c pb_dump.c \
	cm_reset.c cm_cleanup.c cm_close.c \
	prvect.c getp.c monitor.c av_files.c

CC= gcc
RANLIB= ranlib
DEFS=  -DHAVE_STRING_H=1 -DHAVE_STDLIB_H=1 -DHAVE_UNISTD_H=1 -DHAVE_STDARG_H=1 -DHAVE_SGTTY_H=1 -DHAVE_FCNTL_H=1 -DHAVE_MACHINE_ENDIAN_H=1 -DHAVE_SYS_WAIT_H=1 -DHAVE_SYS_TIME_H=1 -DHAVE_SYS_FILE_H=1 -DHAVE_SYS_RESOURCE_H=1 -DTIME_WITH_SYS_TIME=1 -DRETSIGTYPE=void -DDIRENT=1 -DDECLARED_SYS_NERR=1 -DDECLARED_SYS_ERRLIST=1 -DHAVE_STRERROR=1 -DHAVE_STRCSPN=1
MOAN= -Wall
OPTIM= -O
CFLAGS= -DINGRES_CTLMOD -I$(H) $(OPTIM) $(DEFS) $(MOAN)
LINTFLAGS= -huc -I$H
LINT=	lint
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Control Module
CTAGS=	ctags
AR=	-ar
ARFLAGS=rvu
GET=	sccs get
CP=	cp
CHMOD=	chmod
LIBMODE=664

all: libctlmod.a

libctlmod.a: $(OBJS)
	$(AR) $(ARFLAGS) libctlmod.a $(OBJS)
	$(CHMOD) $(LIBMODE) libctlmod.a
	$(RANLIB) libctlmod.a

install: $(LIB)/libctlmod.a

$(LIB)/libctlmod.a: libctlmod.a
	$(CP) libctlmod.a $(LIB)/libctlmod.a
	$(RANLIB) $(LIB)/libctlmod.a

new:
	rm -f libctlmod.a

clean:
	rm -f *.o
	rm -f a.out libctlmod.a core

sources: $(HDRS) $(SRCS1) $(SRCS2)

$(HDRS):
	$(GET) $(REL) $@
$(SRCS1):
	$(GET) $(REL) $@
$(SRCS2):
	$(GET) $(REL) $@

ctags:
	$(CTAGS) *.c

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

lint:
	$(LINT) $(LINTFLAGS) $(SRCS1) $(SRCS2)

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 8)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 8)" *.[hm] *.[csy]
	sed /SCCSID/d < index > tmp
	mv tmp index
	$(VGRIND) -h "$(HEADER) (Release 8)" -x index

tags: sources
	-fgrep ../ tags > othertags
	ctags  $(SRCS1) $(SRCS2) $(HDRS)
	sort -o tags tags othertags
	-rm -f othertags

#
# DANGER	DANGER	DANGER	DANGER	DANGER	DANGER
# The following two entries should NOT be executed by the user.
# These entries are only for the Makefile conf to construct
# the system tag files.
#
systags: 
	ctags $(SRCS1) $(SRCS2) $(HDRS)
	sed "s/	/	ctlmod\//" tags > systags
	cat systags >> ../tags

#
# We assume that systags has already been done.
#
installtags:
	sed "/	ctlmod/d" ../tags | sed "s/	/	..\//" > othertags
	sort -o tags tags othertags
	-rm -f othertags systags


depend:
	grep '^#[ 	]*include' $(SRCS1) $(SRCS2) | sed -n -f ../h/depend.sed > makedep
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

main.o:$H/tree.h
main.o:ctlmod.h
main.o:pipes.h
main.o:$H/resp.h
main.o:$H/ingres.h
main.o:$H/aux.h
main.o:$H/access.h
main.o:$H/lock.h
main.o:$H/sccs.h
main.o:$H/protos.h
error.o:$H/tree.h
error.o:ctlmod.h
error.o:pipes.h
error.o:$H/pv.h
error.o:$H/sccs.h
error.o:$H/protos.h
proc_err.o:$H/tree.h
proc_err.o:ctlmod.h
proc_err.o:pipes.h
proc_err.o:$H/sccs.h
proc_err.o:$H/protos.h
initp.o:ctlmod.h
initp.o:$H/ingres.h
initp.o:$H/aux.h
initp.o:$H/tree.h
initp.o:$H/sccs.h
initp.o:$H/protos.h
setp.o:ctlmod.h
setp.o:$H/ingres.h
setp.o:$H/tree.h
setp.o:$H/aux.h
setp.o:$H/sccs.h
setp.o:$H/protos.h
resetp.o:ctlmod.h
resetp.o:$H/ingres.h
resetp.o:$H/tree.h
resetp.o:$H/sccs.h
resetp.o:$H/protos.h
init_qt.o:ctlmod.h
init_qt.o:$H/ingres.h
init_qt.o:$H/aux.h
init_qt.o:$H/tree.h
init_qt.o:$H/sccs.h
init_qt.o:$H/protos.h
call.o:ctlmod.h
call.o:pipes.h
call.o:$H/resp.h
call.o:$H/ingres.h
call.o:$H/aux.h
call.o:$H/tree.h
call.o:$H/sccs.h
call.o:$H/protos.h
sysdump.o:ctlmod.h
sysdump.o:$H/tree.h
sysdump.o:$H/sccs.h
sysdump.o:$H/protos.h
mapvars.o:ctlmod.h
mapvars.o:$H/tree.h
mapvars.o:$H/ingres.h
mapvars.o:$H/symbol.h
mapvars.o:$H/sccs.h
mapvars.o:$H/protos.h
do_seq.o:$H/tree.h
do_seq.o:ctlmod.h
do_seq.o:pipes.h
do_seq.o:$H/resp.h
do_seq.o:$H/sccs.h
do_seq.o:$H/protos.h
readinput.o:$H/tree.h
readinput.o:ctlmod.h
readinput.o:pipes.h
readinput.o:$H/resp.h
readinput.o:$H/ingres.h
readinput.o:$H/aux.h
readinput.o:$H/tree.h
readinput.o:$H/sccs.h
readinput.o:$H/protos.h
do_st.o:$H/tree.h
do_st.o:ctlmod.h
do_st.o:pipes.h
do_st.o:$H/sccs.h
do_st.o:$H/protos.h
send_off.o:$H/tree.h
send_off.o:ctlmod.h
send_off.o:pipes.h
send_off.o:$H/symbol.h
send_off.o:$H/tree.h
send_off.o:$H/resp.h
send_off.o:$H/ingres.h
send_off.o:$H/aux.h
send_off.o:$H/access.h
send_off.o:$H/sccs.h
send_off.o:$H/protos.h
call_fn.o:$H/tree.h
call_fn.o:ctlmod.h
call_fn.o:$H/resp.h
call_fn.o:$H/sccs.h
call_fn.o:$H/protos.h
readmon.o:$H/tree.h
readmon.o:ctlmod.h
readmon.o:pipes.h
readmon.o:$H/sccs.h
readmon.o:$H/protos.h
readqry.o:ctlmod.h
readqry.o:$H/ingres.h
readqry.o:$H/aux.h
readqry.o:$H/tree.h
readqry.o:$H/symbol.h
readqry.o:$H/sccs.h
readqry.o:$H/errors.h
readqry.o:$H/protos.h
writeqry.o:ctlmod.h
writeqry.o:$H/ingres.h
writeqry.o:$H/aux.h
writeqry.o:$H/tree.h
writeqry.o:$H/symbol.h
writeqry.o:$H/sccs.h
writeqry.o:$H/protos.h
range.o:ctlmod.h
range.o:$H/ingres.h
range.o:$H/symbol.h
range.o:$H/range.h
range.o:$H/tree.h
range.o:$H/sccs.h
range.o:$H/errors.h
range.o:$H/protos.h
treepr.o:ctlmod.h
treepr.o:$H/ingres.h
treepr.o:$H/symbol.h
treepr.o:$H/tree.h
treepr.o:$H/aux.h
treepr.o:$H/sccs.h
treepr.o:$H/protos.h
pb_prime.o:$H/tree.h
pb_prime.o:ctlmod.h
pb_prime.o:pipes.h
pb_prime.o:$H/sccs.h
pb_prime.o:$H/protos.h
pb_get.o:$H/tree.h
pb_get.o:ctlmod.h
pb_get.o:$H/useful.h
pb_get.o:pipes.h
pb_get.o:$H/sccs.h
pb_get.o:$H/protos.h
pb_tput.o:$H/tree.h
pb_tput.o:ctlmod.h
pb_tput.o:pipes.h
pb_tput.o:$H/sccs.h
pb_tput.o:$H/protos.h
pb_put.o:$H/useful.h
pb_put.o:$H/tree.h
pb_put.o:ctlmod.h
pb_put.o:pipes.h
pb_put.o:$H/sccs.h
pb_put.o:$H/protos.h
pb_flush.o:$H/tree.h
pb_flush.o:ctlmod.h
pb_flush.o:pipes.h
pb_flush.o:$H/sccs.h
pb_flush.o:$H/protos.h
pb_read.o:$H/tree.h
pb_read.o:ctlmod.h
pb_read.o:pipes.h
pb_read.o:$H/sccs.h
pb_read.o:$H/protos.h
pb_write.o:$H/tree.h
pb_write.o:ctlmod.h
pb_write.o:pipes.h
pb_write.o:$H/sccs.h
pb_write.o:$H/protos.h
pb_rphys.o:ctlmod.h
pb_rphys.o:pipes.h
pb_rphys.o:$H/sccs.h
pb_rphys.o:$H/protos.h
pb_wphys.o:$H/tree.h
pb_wphys.o:ctlmod.h
pb_wphys.o:$H/useful.h
pb_wphys.o:pipes.h
pb_wphys.o:$H/sccs.h
pb_wphys.o:$H/protos.h
pb_dump.o:ctlmod.h
pb_dump.o:pipes.h
pb_dump.o:$H/sccs.h
pb_dump.o:$H/protos.h
cm_reset.o:ctlmod.h
cm_reset.o:$H/sccs.h
cm_reset.o:$H/protos.h
cm_cleanup.o:ctlmod.h
cm_cleanup.o:$H/sccs.h
cm_cleanup.o:$H/protos.h
cm_close.o:ctlmod.h
cm_close.o:$H/sccs.h
prvect.o:ctlmod.h
prvect.o:$H/tree.h
prvect.o:$H/pv.h
prvect.o:$H/sccs.h
prvect.o:$H/protos.h
getp.o:ctlmod.h
getp.o:$H/pv.h
getp.o:$H/sccs.h
getp.o:$H/protos.h
monitor.o:ctlmod.h
monitor.o:$H/useful.h
monitor.o:$H/pmon.h
monitor.o:$H/sccs.h
monitor.o:$H/protos.h
av_files.o:$H/ingres.h
av_files.o:$H/aux.h
av_files.o:ctlmod.h
av_files.o:$H/sccs.h
av_files.o:$H/protos.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
