#
# makefile for vile.
#
# Not much user-configuration is usually needed.  Various targets in the
# makefile support various systems -- if yours isn't here, and none of the
# one's provided will work, then edit estruct.h, and use "make default".
# For a list of the pre-defined targets, just type "make".
#
# The command/name/key/function bindings are defined in the file "cmdtbl".
# The mktbls program parses this to produce nebind.h, nename.h, and
# nefunc.h, which are used by the rest of the build.
#
# Buffer/window modes are defined in the file "modetbl".
# The mktbls program parses this to produce nemode.h, which is included in
# 'estruct.h'.
#
# The version number is found near the top of edef.h, and is displayed with
# the '*' and ":version" commands, or by invoking vile with "-V".
#
# Paul Fox
#
# original makefile for uemacs: Adam Fritz July 30,1987  (do you recognize it?)
#

# some old make's don't predefine this:
#MAKE=/bin/make
#MAKE=/usr/bin/make

# To change screen driver modules, change SCREEN and SCRDEF below, OR edit
# estruct.h to make sure the correct one is #defined as "1", and the others
# all as "0".  If you use tcap.c, you'll need libtermcap.a too.  If you use
# x11.c, you'll need libX11.a too.

# for regular vile, use these:
SCREEN = tcap
LIBS = -ltermcap
TARGET = vile
SCRDEF = -DTERMCAP -Dscrn_chosen

# for building the X version, xvile, use these:
#  note that if you switch from "vile" to "xvile" or back, you _must_ do
#  a "make clean" in between.
#SCREEN = x11
#LIBS = -lX11
#TARGET = xvile
#SCRDEF = -DX11 -Dscrn_chosen

CO=co
#CO="@echo co"

# for passing along the above settings to sub-makes
ENVIR = SCREEN="$(SCREEN)" LIBS="$(LIBS)" TARGET="$(TARGET)" \
	SCRDEF="$(SCRDEF)" \
	CO="$(CO)" CC="$(CC)" LINK="$(LINK)" OPTFLAGS="$(OPTFLAGS)"

default: bsd386


DESTDIR = /usr/local/bin

# if you want the help file (vile.hlp) to go somewhere other than your $PATH
#  or one of the hard-code paths in epath.h  (it goes to the same place vile
#  does by default)
# note you may have to hardcode this if the CFLAGS definition below doesn't
#  get through your make.
#HELP_LOC=/my/local/help/lib
 HELP_LOC=

REMOTE=gutso!foxharp

#CC = gcc
#OPTFLAGS = -g -O -Wall -Wshadow # -Wconversion -Wstrict-prototypes -Wmissing-prototypes

CC = cc
#OPTFLAGS = -g
OPTFLAGS = -O

#LINK = $(CC)
LINK = $(CC)
LDFLAGS =

INCS =

# the backslashes around HELP_LOC don't make it through all makes.  if
#  you have trouble, use the following line instead, and edit epath.h to
#  hardcode your HELP_LOC path if you need one.
# CFLAGS0 = $(INCS) $(SCRDEF)
CFLAGS0 = $(INCS) $(SCRDEF) -DHELP_LOC=\\\"$(HELP_LOC)\\\"
CFLAGS1 = $(OPTFLAGS) $(CFLAGS0)

# suffix for object files.
# this get changes to "obj" for DOS builds
O = o

# All of the makefiles which should be preserved and distributed
UNIXMAK = Makefile 				# on UNIX
VMSMAK = descrip.mms vms_link.opt		# on VMS
TURBOMAK = makefile.tbc				# on DOS, using TURBO
WATMAK = makefile.wat				# on DOS, using Watcom C/386
MSCMAK =	# still waiting for this one	# on DOS, using Microsoft C
DJGPPMAK = makefile.djg				# on DOS, DJGCC v1.09
MAKFILES = $(UNIXMAK) $(VMSMAK) $(TURBOMAK) $(WATMAK) $(DJGPPMAK) $(MSCMAK)

MKTBLS = ./mktbls

ALLTOOLS = $(MAKFILES)


# these are normal editable headers
HDRS = estruct.h epath.h edef.h proto.h dirstuff.h glob.h

# these headers are built by the mktbls program from the information in cmdtbl
# and in modetbl
BUILTHDRS = nebind.h nefunc.h nemode.h nename.h nevars.h

ALLHDRS = $(HDRS)

# All the C files which should be saved
#  (including tools, like mktbls.c, unused screen drivers, etc.)
CSRCac = ansi.c at386.c basic.c bind.c buffer.c crypt.c csrch.c
CSRCde = dg10.c display.c eval.c exec.c externs.c
CSRCf = fences.c file.c filec.c fileio.c finderr.c
CSRCgh = glob.c globals.c history.c hp110.c hp150.c
CSRCil = ibmpc.c input.c insert.c isearch.c line.c
CSRCm = main.c map.c modes.c mktbls.c
CSRCnr = npopen.c opers.c oneliner.c path.c random.c regexp.c region.c
CSRCst = search.c spawn.c st520.c tags.c tbuff.c tcap.c termio.c tipc.c tmp.c
CSRCuv = undo.c version.c vmalloc.c vms2unix.c vmspipe.c vmsvt.c vt52.c
CSRCw = window.c word.c wordmov.c
CSRCxz = x11.c z309.c z_ibmpc.c

CSRC = $(CSRCac) $(CSRCde) $(CSRCf) $(CSRCgh) $(CSRCil) $(CSRCm) $(CSRCnr) \
	$(CSRCst) $(CSRCuv) $(CSRCw) $(CSRCxz)

# non-C source code
OTHERSRC = z100bios.asm

# text and data files
TEXTFILES = README CHANGES cmdtbl modetbl vile.hlp vile.1 buglist revlist \
	README.X11

ALLSRC = $(CSRC) $(OTHERSRC)

EVERYTHING = $(ALLTOOLS) $(ALLHDRS) $(ALLSRC) $(TEXTFILES) $(SHORTSTUFF)


SRC = main.c $(SCREEN).c basic.c bind.c buffer.c crypt.c \
	csrch.c display.c eval.c exec.c externs.c \
	fences.c file.c filec.c \
	fileio.c finderr.c glob.c globals.c history.c \
	input.c insert.c isearch.c \
	line.c map.c modes.c npopen.c oneliner.c \
	opers.c path.c random.c regexp.c \
	region.c search.c spawn.c tags.c tbuff.c \
	termio.c tmp.c undo.c \
	version.c vmalloc.c window.c word.c wordmov.c

OBJ = main.$O $(SCREEN).$O basic.$O bind.$O buffer.$O crypt.$O \
	csrch.$O display.$O eval.$O exec.$O externs.$O \
	fences.$O file.$O filec.$O \
	fileio.$O finderr.$O glob.$O globals.$O history.$O \
	input.$O insert.$O isearch.$O \
	line.$O map.$O modes.$O npopen.$O oneliner.$O \
	opers.$O path.$O random.$O regexp.$O \
	region.$O search.$O spawn.$O tags.$O tbuff.$O \
	termio.$O tmp.$O undo.$O \
	version.$O vmalloc.$O window.$O word.$O wordmov.$O


# if your pre-processor won't treat undefined macros as having value 0, or
#	won't give unvalue'd defines the value 1, you'll have to do your
#	config inside of estruct.h, and use "make default"
# otherwise, there are essentially four choices, unless your machine is
#	one of the more "specific" targets.  you can be either att or bsd, with
#	or without posix extensions.

# please report bugs with these config options
all:
	make bsd386

_all:
	@echo "	there is no default unnamed target."			;\
	echo "	please use one of the following:"			;\
	echo "	make bsd	(for pure, older BSD systems)"		;\
	echo "	make bsd_posix	(for BSD with some POSIX support)"	;\
	echo "	make bsd386"						;\
	echo "	make att	(traditional USG systems)"		;\
	echo "	make att_posix	(newer, with POSIX support)"		;\
	echo "	make sgi	(Silicon Graphics)"			;\
	echo "	make svr3	(early 386 UNIX, for instance)"		;\
	echo "	make sunos	(sunos 3 or 4)"				;\
	echo "	make ultrix"						;\
	echo "	make mach	(just pure bsd)"			;\
	echo "	make svr4	(Solaris 2.1, 2.2)"			;\
	echo "	make mips	(uses systemV stuff)"			;\
	echo "	make odt	(SCO Open DeskTop -- variant of svr3)"	;\
	echo "	make isc	(interactive -- another such variant)"	;\
	echo "	make hpux"						;\
	echo "	make next	(NeXT)"					;\
	echo "	make sony	(Sony News -- very BSD)"		;\
	echo "	make unixpc	(AT&T 3B1)"				;\
	echo "	make uts	(Amdahl UTS 2.1.5)"			;\
	echo "	make aix	(but probably only rs6000)"		;\
	echo "	make osf1	(OSF/1)"				;\
	echo "	make linux	(ported to 0.95)"			;\
	echo "	make aux2	(A/UX 2.0) (3.0 is probably svr3)"	;\
	echo "	make apollo	(HP/Apollo SR10.3 CC 6.8)"		;\
	echo "	nmake msc	(MicroSoft C 6.0) (buggy?)"		;\
	echo "	make sx1100	(SX1100 running on Unisys 1100)"	;\
	echo "	make default	(to use config internal to estruct.h)"

bsd sony mach:
	$(MAKE) CFLAGS="$(CFLAGS1) -DBERK -Dos_chosen" \
	    MAKE=/usr/bin/make $(TARGET) $(ENVIR)

bsd_posix ultrix:
	$(MAKE) CFLAGS="$(CFLAGS1) -DBERK -DPOSIX -DULTRIX -Dos_chosen" \
		$(TARGET) $(ENVIR)

sunos:
	$(MAKE) CFLAGS="$(CFLAGS1) -DBERK -DPOSIX -DSUNOS -Dos_chosen" \
		$(TARGET) $(ENVIR)

bsd386:
	make CFLAGS="$(CFLAGS1) -DBERK -DBSD386 -Dos_chosen" \
	    MAKE=/usr/bin/make $(TARGET) $(ENVIR)

att:
	$(MAKE) CFLAGS="$(CFLAGS1) -DUSG -Dos_chosen" \
		$(TARGET) $(ENVIR)

att_posix svr4:
	$(MAKE) CFLAGS="$(CFLAGS1) -DUSG -DPOSIX -Dos_chosen" \
		$(TARGET) $(ENVIR)

sgi:
	@echo "Use 'make att_posix' after adding "-D__STDC__" to OPT_FLAGS"

svr3:
	$(MAKE) CFLAGS="$(CFLAGS1) -DSVR3 -Dos_chosen" \
		$(TARGET) $(ENVIR)

mips:
	$(MAKE) CFLAGS="$(CFLAGS1) -systype sysv $(INCS) -DSVR3 -DMIPS \
		-Dos_chosen" \
		$(TARGET) $(ENVIR)

odt:
	$(MAKE) \
	CFLAGS="$(CFLAGS1) -DODT -DUSG -DPOSIX -DSVR3_PTEM -Dos_chosen" \
		$(TARGET) $(ENVIR)

isc:
	$(MAKE) \
	CFLAGS="$(CFLAGS1) -DISC -DUSG -DPOSIX -DSVR3_PTEM -Dos_chosen" \
		$(TARGET) $(ENVIR)

hpux:
	$(MAKE) CFLAGS="$(CFLAGS1) -DUSG -DHAVE_SELECT -DHPUX -Dos_chosen" \
		$(TARGET) $(ENVIR)

next:
	$(MAKE) CFLAGS="$(CFLAGS1) -DBERK -DNeXT -D__STRICT_BSD__ -Dos_chosen \
		-I/NextDeveloper/Headers -I/NextDeveloper/Headers/bsd \
		-I/NextDeveloper/Headers/bsd/sys" \
		$(TARGET) $(ENVIR)

unixpc:
	$(MAKE) CFLAGS="$(CFLAGS1) -DUSG -DHAVE_SELECT \
		-DHAVE_MKDIR=0 -Dwinit=xxwinit -Dos_chosen -DUNIXPC" \
		$(TARGET) $(ENVIR)

uts:		# use -eft to allow editing terabyte files.  :-)
	$(MAKE) CFLAGS='$(CFLAGS1) -DUSG -eft -Dos_chosen' \
		$(TARGET) $(ENVIR)

# i've had reports that the curses lib is broken under AIX.  on the other
#  hand, the termcap lib doesn't appear in all versions of AIX, and when it
#  does, it limits you to an 80x24 window.  since I don't have any AIX machine
#  to try this on, i'm afraid you may have to experiment
# in addition, since I didn't know that aix varies so much from platform to
#  platform, i used "AIX" as the ifdef in the code.  it may not work for
#  all the other platforms.  sorry.
aix:
	$(MAKE) CFLAGS="$(CFLAGS1) -DUSG \
		-DPOSIX -DAIX -DHAVE_SELECT -U__STR__ -Dos_chosen -qpgmsize=l" \
		LIBS=-lcurses \
		$(TARGET) $(ENVIR)

apollo:
	$(MAKE) CFLAGS="$(CFLAGS1) -DBERK -DAPOLLO -Dos_chosen" \
	    MAKE=/usr/bin/make $(TARGET) $(ENVIR)

osf1:
	$(MAKE) CFLAGS="$(CFLAGS1) -DBERK -DPOSIX -DOSF1 -Dos_chosen" \
		$(TARGET) $(ENVIR)

linux:
	$(MAKE) CFLAGS="$(CFLAGS1) -DUSG \
		-DPOSIX -DHAVE_SELECT -DHAVE_POLL=0 -DLINUX -Dos_chosen" \
		$(TARGET) $(ENVIR)

# It has been pointed out to me that the stock COMMAND.COM can't possibly
# handle the length of the commands that this causes.  You may have to
# create your own makefile for DOS.  I think it's only this top level
# re-invocation of make that's a problem -- everything else should fit in
# 128 characters.  I think.  (It works as-is under SoftPC, which is why
# I wrote it this way.)
msc:
	$(MAKE) MKTBLS=mktbls.exe CFLAGS="/qc /AL /nologo \
		-DMSDOS -DMSC -Dos_chosen -DIBMPC -Dscrn_chosen" \
		O=obj CC=cl SCREEN=ibmpc \
		LNKFILE=link.msc \
		LINK="link /MAP /CO /NOI /STACK:4096 @link.msc" $(TARGET)2

aux2:
	$(MAKE) CFLAGS="$(CFLAGS1) -DUSG -DAUX2 -Dos_chosen" \
		$(TARGET) $(ENVIR)

sx1100:
	$(MAKE) CFLAGS="$(CFLAGS1) -DSVR3 -Dos_chosen" \
		LDFLAGS="-h 400000" \
		$(TARGET) $(ENVIR)

default:
	$(MAKE) CFLAGS="$(CFLAGS1) -Uos_chosen" \
		$(TARGET) $(ENVIR)

$(TARGET): $(BUILTHDRS) $(OBJ) Makefile
	-mv $(TARGET) o$(TARGET)
	$(LINK) $(LDFLAGS) -o $(TARGET) $(OBJ) $(LIBS)
#	$(LINK) -pg -o $(TARGET) $(OBJ) $(LIBS) -lc_p
#	$(LINK) -Bstatic -o $(TARGET) $(OBJ) $(LIBS)

# this target is more convenient for the nonstandard environments, like DOS
$(TARGET)2: $(BUILTHDRS) $(OBJ) Makefile
	-mv $(TARGET) o$(TARGET)
	$(LINK)

# this has to be run on a unix system, unfortunately.
#  once on dos, you'll have to maintain link.msc by hand
link.msc: Makefile
	@echo +>$@
	@(echo $(OBJ) | xargs -n5 ) | \
		sed -e 's/$(SCREEN)/ibmpc/' \
		    -e 's/\.o/\.obj/g' \
		    -e 's/$$/+/' >> $@
	@echo >> $@
	@echo vile.exe >> $@
	@echo vile.map\; >> $@

# end of file

saber_src:
	#load $(CFLAGS) $(SRC) $(LIBS)

saber_obj: $(OBJ)
	#load $(CFLAGS) $(OBJ) $(LIBS)

#.c.src:
#	#load $(CFLAGS) $<
#.o.obj:
#	#load $(CFLAGS) $<
#
#SUFFIXES: .c .h .o .src .obj


nebind.h \
nefunc.h \
nename.h :	cmdtbl $(MKTBLS)
	$(MKTBLS) cmdtbl

nevars.h \
nemode.h:	modetbl $(MKTBLS)
	$(MKTBLS) modetbl

$(MKTBLS):  mktbls.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(MKTBLS)  mktbls.c

# install to DESTDIR
install:
	-@[ -x $(TARGET) ] || (echo must make $(TARGET) first && exit 1)
	@[ -d $(DESTDIR) ] || mkdir -p $(DESTDIR)
	@echo Installing $(TARGET) to $(DESTDIR) ; cp $(TARGET) $(DESTDIR) 
	@echo Installing vile.hlp to $(DESTDIR) ; cp vile.hlp $(DESTDIR)
	@chmod 0644 $(DESTDIR)/vile.hlp

package:	install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST vile_bin

compr-shar: link.msc /tmp/vilevers
	[ -d cshardir ] || mkdir cshardir
#	add -a for archive headers, add -s pgf@cayman.com for submitted-by
	vilevers=`cat /tmp/vilevers`; \
	shar -p -nvile$${vilevers} -L55 -o cshardir/vileshar \
		-T README -C `ls $(EVERYTHING) | sed '/^README$$/d'` link.msc

shar: link.msc /tmp/vilevers
	[ -d shardir ] || mkdir shardir
#	add -a for archive headers, add -s pgf@cayman.com for submitted-by
	vilevers=`cat /tmp/vilevers`; \
	shar -x -a -spgf@cayman.com -nvile$${vilevers} -L55 \
			-o shardir/vileshar `ls $(EVERYTHING)` link.msc

bigshar: link.msc /tmp/vilevers
	@echo 'Do you need to rebuild the revlist????'
	vilevers=`cat /tmp/vilevers`; \
	shar -spgf@cayman.com -nvile$${vilevers} \
	    -o vile$${vilevers}shar README `ls $(EVERYTHING) | \
	    sed '/^README$$/d'` link.msc ; \
	mv vile$${vilevers}shar.01 vile$${vilevers}shar

patch:	link.msc /tmp/vilevers
	@orevlistrev=`rlog -h revlist | egrep head: | cut -f2 -d'.'`	;\
	orevlistrev=1.`expr $$orevlistrev - 1`				;\
	ovilevers=`cat /tmp/vilevers | cut -f2 -d'.'`			;\
	ovilemajor=`cat /tmp/vilevers | cut -f1 -d'.'`			;\
	ovilevers=$$ovilemajor.`expr $$ovilevers - 1`			;\
	echo Previous version is $$ovilevers				;\
	vilevers=`cat /tmp/vilevers`					;\
	co -p$$orevlistrev revlist |					 \
	while read file filerev						;\
	do								 \
	  rcsdiff -c -r$$filerev $$file 2>/dev/null || true		;\
	done  >patch$$ovilevers-$$vilevers 

/tmp/vilevers: ALWAYS
	@expr "`egrep 'version\[\].*' edef.h`" : \
		'.* \([0-9][0-9]*\.[0-9].*\)".*' >/tmp/vilevers
	@echo Current version is `cat /tmp/vilevers`

# only uucp things changed since last time
uuto:
	uuto `ls -t $(EVERYTHING) uutodone | sed '/uutodone/q'` $(REMOTE)
	date >uutodone

uurw:
	uuto `make rw` $(REMOTE)

rcsdiffrw:
	@-for x in `$(MAKE) rw`	;\
	do	\
		echo 		;\
		echo $$x	;\
		echo =========	;\
		rcsdiff $$x	;\
	done 2>&1		;\
	echo			;\
	echo all done

floppy:
	ls $(EVERYTHING) | oo

# you don't want to know...
dosscript:
	(								\
	echo echo on							;\
	for x in `ls -t $(EVERYTHING) dosback.bat | sed '/dosback.bat/q'` ;\
	do								\
		echo copy u:$$x a:					;\
	done 								;\
	#echo quit							;\
	) >tmp.bat
	ud < tmp.bat >dosback.bat
	/bin/rm -f tmp.bat
	#-dos
	#>dosback.bat

newdosfloppy:
	touch 0101010170 dosback.bat

# dump a list of the important files
list:
	@ls $(EVERYTHING) | more

# make a list of RCS revisions.  don't include the revlist itself
nrevlist:
	$(MAKE) list  | egrep -v revlist >/tmp/vile__files
	rlog -Ntrunk `cat /tmp/vile__files` >/tmp/vile__revs
	-paste /tmp/vile__files /tmp/vile__revs >/tmp/vile_revlist
	rm /tmp/vile__files /tmp/vile__revs
	mv /tmp/vile_revlist nrevlist

nchanges: nrevlist
	revlistdiff nrevlist revlist >changes


# dump a list of files that may have changed since last backup
rw list-writeable:
	@ls -l $(EVERYTHING) | \
		egrep '^[^l].w' | \
		sed 's;.* ;;'   # strip to last space

no-write:
	chmod -w $(EVERYTHING)

update:
	nupdatefile.pl -r $(EVERYTHING)

protos:
	cextract -D__STDC__ +E +P +s +r -o nproto.h $(SRC)

TAGS tagfile:
	dotags $(SRC) $(HDRS)

lint:	$(SRC)
	#lint -hbvxac $(SRC) >lint.out
	#lint $(CFLAGS0) $(ENVIR) $(SRC) >lint.out
	#lint $(CFLAGS0) -DBERK -DPOSIX -DSUNOS -Dos_chosen  $(SRC) >lint.out
	lint $(CFLAGS0) -DSVR3 -Dos_chosen  $(SRC) >lint.out

cflow:	$(SRC)
	cflow  $(SRC) >cflow.out

clean:
	rm -f *.$O o$(TARGET) $(BUILTHDRS) $(MKTBLS) core *~ *.BAK

clobber: clean
	rm -f $(TARGET)

print:
	pr Makefile $(HDRS) $(SRC) | lpr

depend:	 $(SRC) $(HDRS) $(BUILTHDRS)
	mv -f Makefile Makefile.orig
	(sed -e '/^#DEPENDS/,$$d' Makefile.orig ; \
		echo "#DEPENDS" ; \
		$(CC) -M $(CFLAGS) $? ) > Makefile

populate: $(EVERYTHING)

$(EVERYTHING):
	$(CO) -r$(revision) $@


$(OBJ): estruct.h nemode.h edef.h proto.h

ALWAYS:

main.$O:	nevars.h glob.h
bind.$O:	epath.h
eval.$O:	glob.h
filec.$O:	dirstuff.h
eval.$O:	nevars.h
glob.$O:	dirstuff.h glob.h
externs.$O:	nebind.h nename.h nefunc.h
path.$O:	dirstuff.h
random.$O:	glob.h
vmalloc$O:	nevars.h

# $Log: Makefile,v $
# Revision 1.4  1994/04/17 12:48:13  jkh
# Fixed some bogus Makefiles.
#
# Revision 1.3  1994/04/11  14:59:00  jkh
# rename pkg target to package to make Julian S. happy.
#
# Revision 1.2  1994/02/25  03:15:50  jkh
# Added a package for vile.
#
# Revision 1.1  1994/02/22  06:34:41  jkh
# Added a more kosher Makefile that doesn't use all kinds of peculiar
# magic tricks to fail in interesting ways..
#
# Revision 1.1.1.1  1994/02/01  03:29:32  jkh
# This is vile, VI-like emacs (he said it, I didn't).
#
# Revision 1.120  1993/09/03  09:11:54  pgf
# tom's 3.60 changes
#
# Revision 1.119  1993/08/18  18:48:49  pgf
# added patch: target, for creating release patches
#
# Revision 1.118  1993/08/16  14:04:39  pgf
# fixed broken all: target -- missing double quote
#
# Revision 1.117  1993/08/13  16:24:47  pgf
# added sgi target (sort of)
#
# Revision 1.116  1993/07/28  10:44:51  pgf
# comment about "make clean" when doing xvile
#
# Revision 1.115  1993/07/19  10:39:26  pgf
# svr4 now tested
#
# Revision 1.114  1993/07/15  10:37:58  pgf
# see 3.55 CHANGES
#
# Revision 1.113  1993/07/08  10:41:15  pgf
# turn off the strict gcc warnings by default -- we don't pass yet.
#
# Revision 1.112  1993/07/06  16:55:58  pgf
# added makefile.djg for dj gcc
#
# Revision 1.111  1993/06/25  15:05:12  pgf
# added watcom makefile, renamed the turbo makefiles
#
# Revision 1.110  1993/06/18  15:57:06  pgf
# tom's 3.49 changes
#
# Revision 1.109  1993/06/10  14:57:25  pgf
# added map.c
#
# Revision 1.108  1993/06/02  14:46:13  pgf
# added more gcc warnings
#
# Revision 1.107  1993/05/11  16:22:22  pgf
# see tom's CHANGES, 3.46
#
# Revision 1.106  1993/05/06  16:21:28  pgf
# uts compile-line change
#
# Revision 1.105  1993/05/06  10:48:29  pgf
# mention sco in the make help
#
# Revision 1.104  1993/05/05  12:28:57  pgf
# added some -DMACHINE defines to the compile lines (needed -DULTRIX)
#
# Revision 1.103  1993/04/29  19:19:16  pgf
# added Turbo-C makefiles, and vile.1 man page
#
# Revision 1.102  1993/04/28  14:34:11  pgf
# see CHANGES, 3.44 (tom)
#
# Revision 1.101  1993/04/28  09:42:25  pgf
# add -O back into gcc optflags, so gcc will warn of unused variables
#
# Revision 1.100  1993/04/20  12:18:32  pgf
# see tom's 3.43 CHANGES
#
# Revision 1.99  1993/04/02  15:23:36  pgf
# more permissible in naming bigshar file
#
# Revision 1.98  1993/04/02  11:01:50  pgf
# dependency updates
#
# Revision 1.97  1993/04/01  14:44:30  pgf
# added -DNeXT for NeXT machines
#
# Revision 1.96  1993/04/01  12:55:37  pgf
# added vmspipe.c
#
# Revision 1.95  1993/04/01  12:12:13  pgf
# took out GINCS stuff, and the -I for /usr/include and /usr/include/sys.  they
# were interfering with the gcc fixincludes directories
#
# Revision 1.94  1993/03/31  19:41:08  pgf
# added revlist reminder to bigshar, took -O out of gcc line -- it confuses
# me too much in gdb
#
# Revision 1.93  1993/03/25  19:50:58  pgf
# see 3.39 section of CHANGES
#
# Revision 1.92  1993/03/17  10:43:53  pgf
# made building/naming shar files easier -- gets version automatically
#
# Revision 1.91  1993/03/17  10:00:29  pgf
# initial changes to make VMS work again
#
# Revision 1.90  1993/03/16  10:53:21  pgf
# see 3.36 section of CHANGES file
#
# Revision 1.89  1993/03/08  15:24:00  pgf
# added install2 target, which typically installs to your home directory
#
# Revision 1.88  1993/03/05  17:50:54  pgf
# see CHANGES, 3.35 section
#
# Revision 1.87  1993/02/24  10:59:02  pgf
# see 3.34 changes, in CHANGES file
#
# Revision 1.86  1993/02/24  09:31:53  pgf
# warning re: HELP_LOC backslashes, uts support, and better (?) NeXT support
#
# Revision 1.85  1993/02/15  10:50:47  pgf
# added aix warnings
#
# Revision 1.84  1993/02/15  10:36:34  pgf
# back to cc
#
# Revision 1.83  1993/02/12  10:46:18  pgf
# changed name of ENV to ENVIR, due to name conflict with linux make
#
# Revision 1.82  1993/02/08  14:53:35  pgf
# see CHANGES, 3.32 section
#
# Revision 1.81  1993/01/23  13:38:23  foxharp
# nevars.h replaces evar.h
#
# Revision 1.80  1993/01/12  08:44:24  foxharp
# fixup so README.X11 gets included in shars, and more lint rules from tom dickey
#
# Revision 1.79  1992/12/29  23:17:28  foxharp
# commentary
#
# Revision 1.78  1992/12/28  23:51:57  foxharp
# test for presence of vile in destdir before trying to move it to ovile
#
# Revision 1.77  1992/12/16  21:38:34  foxharp
# rule for nchanges target
#
# Revision 1.76  1992/12/14  09:02:08  foxharp
# svr3 lint command
#
# Revision 1.75  1992/12/13  13:27:04  foxharp
# various changes, for easier command-line ENVIR setting, and for linting
#
# Revision 1.74  1992/12/04  09:50:24  foxharp
# pass SCREEN etc. to lower makes via ENV variable
#
# Revision 1.73  1992/12/04  09:23:08  foxharp
# added apollo
#
# Revision 1.72  1992/11/19  09:14:41  foxharp
# added HELP_LOC support -- allows easy choice of new dest for vile.hlp
#
# Revision 1.71  1992/08/28  08:55:08  foxharp
# switch from -g to -O for releaseh
#
# Revision 1.70  1992/08/20  23:40:48  foxharp
# typo fixes -- thanks, eric
#
# Revision 1.69  1992/08/20  08:58:05  foxharp
# final cleanup preparing to release version four
#
# Revision 1.68  1992/08/07  18:05:27  pgf
# fixed mips systype botch
#
# Revision 1.67  1992/08/04  20:12:00  foxharp
# split out sunos, so SUNOS gets defined, added comment about DOS command
# line lengths, and consolidated some vars into CFLAGS1
#
# Revision 1.66  1992/08/03  09:10:29  foxharp
# added eric krohn's changes for sx1100 -- introduced LDFLAGS
#
# Revision 1.65  1992/07/24  18:22:06  foxharp
# change name of MAKEFILES -- that name is special to GNU make
#
# Revision 1.64  1992/07/22  00:50:32  foxharp
# turn off shared libs
#
# Revision 1.63  1992/07/20  22:48:17  foxharp
# profiling support (commented out)
#
# Revision 1.62  1992/07/17  19:21:53  foxharp
# added gcc sys dir to find filio.h, and changed "sun" to "sunos" to make
# way for solaris
#
# Revision 1.61  1992/07/16  22:05:01  foxharp
# fixed linux rule -- missing -DLINUX
#
# Revision 1.60  1992/07/01  17:03:12  foxharp
# added z_ibmpc.c
#
# Revision 1.59  1992/06/25  23:00:50  foxharp
# changes for dos/ibmpc
#
# Revision 1.58  1992/06/22  08:40:02  foxharp
# added INCS to all targets, especially aix, so it'll get ioctl.h in termio.c
#
# Revision 1.57  1992/06/14  11:33:17  foxharp
# added -DAIX
#
# Revision 1.56  1992/06/12  20:21:28  foxharp
# use shared libs in svr3 -- should this be done for others?  isc? odt?
#
# Revision 1.55  1992/06/04  19:44:40  foxharp
# added mach and cflow targets
#
# Revision 1.54  1992/06/03  08:39:23  foxharp
# look explicitly for gcc include files
#
# Revision 1.53  1992/06/01  20:41:07  foxharp
# added dependency of externs.o on ne....h
#
# Revision 1.52  1992/05/29  09:40:53  foxharp
# split out modes.c, fences.c, and insert.c from random.c
#
# Revision 1.51  1992/05/29  08:40:05  foxharp
# minor ordering
#
# Revision 1.50  1992/05/27  19:16:22  foxharp
# fixed odt and isc targets -- added -DODT and -DISC
#
# Revision 1.49  1992/05/27  08:32:57  foxharp
# added rcsdiffrw target
#
# Revision 1.48  1992/05/20  18:58:11  foxharp
# comments on target help, a/ux support, and uurw
#
# Revision 1.47  1992/05/19  09:15:45  foxharp
# machine target fixups
#
# Revision 1.46  1992/05/16  12:00:31  pgf
# prototypes/ansi/void-int stuff/microsoftC
#
# Revision 1.45  1992/05/13  09:14:12  pgf
# comment changes
#
# Revision 1.44  1992/04/26  13:42:33  pgf
# added bsd386 target
#
# Revision 1.43  1992/04/14  08:55:38  pgf
# added support for OSF1 (affects termio only)
#
# Revision 1.42  1992/04/10  18:52:21  pgf
# add config targets for various platforms
#
# Revision 1.41  1992/04/06  09:43:55  pgf
# fixed nrevlist target
#
# Revision 1.40  1992/04/06  09:29:57  pgf
# added nrevlist target
#
# Revision 1.39  1992/04/06  09:21:40  pgf
# added revlist
#
# Revision 1.38  1992/04/02  23:01:10  pgf
# re-alphabetized SRC and OBJ
#
# Revision 1.37  1992/03/26  09:16:16  pgf
# added regexp.c dependencies
#
# Revision 1.36  1992/03/25  19:12:19  pgf
# explicit rule for building mktbls, to satisfy broken BSD make
#
# Revision 1.35  1992/03/19  23:36:42  pgf
# removed shorten stuff
#
# Revision 1.34  1992/03/07  10:25:58  pgf
# AIX support (needs -lcurses)
#
# Revision 1.33  1992/03/01  18:41:31  pgf
# moved target define
#
# Revision 1.32  1992/02/17  08:48:49  pgf
# keep copy of current executable during install
#
# Revision 1.31  1991/12/30  23:15:04  pgf
# rename the product of bigshar
#
# Revision 1.30  1991/11/27  10:17:21  pgf
# changes to dos backup target
#
# Revision 1.29  1991/11/16  18:35:08  pgf
# dropped the file locking files -- they didn't work, and were only
# compatible with other's running vile
#
# Revision 1.28  1991/11/13  20:09:27  pgf
# X11 changes, from dave lemke
#
# Revision 1.27  1991/11/07  02:00:32  pgf
# lint cleanup
#
# Revision 1.26  1991/11/01  14:56:51  pgf
# took tags file out of the distribution
#
# Revision 1.25  1991/11/01  14:20:24  pgf
# a little bit more saber support
#
# Revision 1.24  1991/10/28  14:19:46  pgf
# took out some old junk, moved the changelog down low
#
# Revision 1.23  1991/10/27  01:47:14  pgf
# switched from regex to regexp
#
# Revision 1.22  1991/10/24  13:03:48  pgf
# added regex.c
#
# Revision 1.21  1991/10/23  12:05:37  pgf
# added bigshar, and put ./ in front of mktbls rule -- there seems
# to be a bug in some makes that drops the ./ in the MKTBLS
# variable
#
# Revision 1.20  1991/10/22  14:36:01  pgf
# added the CHANGES file
#
# Revision 1.19  1991/10/10  12:20:26  pgf
# added vmalloc.o dependencies
#
# Revision 1.18  1991/09/27  02:49:30  pgf
# initial saber support
#
# Revision 1.17  1991/08/10  01:24:28  pgf
# added update target, and
# removed the BUILTHDRS list
#
# Revision 1.16  1991/08/07  11:56:58  pgf
# added RCS log entries
#
# revision 1.15
# date: 1991/08/07 02:10:53;
# removed './' from files in dependencies
#
# revision 1.14
# date: 1991/08/06 16:19:55;
# added populate rule, and "revision" arg to co rule
#
# revision 1.13
# date: 1991/08/06 16:09:42;
# fixed "rw" rule
#
# revision 1.12
# date: 1991/08/06 14:45:14;
# commented out shortnames stuff, and
# added "co" rule
#
# revision 1.11
# date: 1991/06/28 10:52:36;
# exclude built headers from come lists
#
# revision 1.10
# date: 1991/06/16 17:29:07;
# fixed install rules, so I don't have to change it at home
#
# revision 1.9
# date: 1991/06/04 16:00:40;
# switch to -L for shar'ing
#
# revision 1.8
# date: 1991/06/04 13:12:25;
# cleanup for release of version three
#
# revision 1.7
# date: 1991/04/08 13:11:17;
# added 'rw' target, and make some dos backup changes
#
# revision 1.6
# date: 1991/04/04 09:38:12;
# new REMOTE address
#
# revision 1.5
# date: 1990/12/06 18:53:14;
# fixed compr-shar entry, commented out remap.h target
#
# revision 1.4
# date: 1990/12/03 12:01:37;
# comment change
#
# revision 1.3
# date: 1990/10/03 16:09:30;
# shortened "shortnames" to "shorten" !
#
# revision 1.2
# date: 1990/10/01 12:32:23;
# added shortnames directory
#
# revision 1.1
# date: 1990/09/21 10:25:38;
# initial vile RCS revision
#

# (dickey's rules)
CPP_OPTS= $(CFLAGS0) -DBERK -DAPOLLO -Dos_chosen
#CPP_OPTS= $(CFLAGS0) -DBERK -DSUNOS -DPOSIX -Dos_chosen
.SUFFIXES: .i .i2 .lint

lintlib::	llib-lVi1.ln
llib-lVi1.ln:	llib-lVi1 $(ALLHDRS)	; makellib $(CPP_OPTS) llib-lVi1 Vi1
llib-lVi1:	$(BUILTHDRS) $(SRC)	; cproto -l $(CPP_OPTS) `find $(SRC) -print |grep '^[a-k]'` >$@

lintlib::	llib-lVi2.ln
llib-lVi2.ln:	llib-lVi2 $(ALLHDRS)	; makellib $(CPP_OPTS) llib-lVi2 Vi2
llib-lVi2:	$(BUILTHDRS) $(SRC)	; cproto -l $(CPP_OPTS) `find $(SRC) -print |grep '^[l-z]'` >$@

lint.out:	$(BUILTHDRS) ;tdlint $(CPP_OPTS) $(LIBS) $(SRC) >$@

.c.i:		;$(CC)  $(CPP_OPTS) -C -E $< >$@
.c.i2:		;/usr/lib/cpp  $(CPP_OPTS) -C $< >$@
.c.lint:	;tdlint -lVi1 -lVi2 $(LIBS) $(CPP_OPTS) $< >$@

