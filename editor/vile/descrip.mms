#
# VMS makefile for vile.  Requires "MMS"
#
# Tested with:
#	VMS system version 5.4-2
#	MMS version 2.6
#	VAX-C version 3.2
#
# To change screen driver modules, change SCREEN and SCRDEF below, OR edit
# estruct.h to make sure the correct one is #defined as "1", and the others
# all as "0".  If you use tcap.c, you'll need libtermcap.a too.  If you use
# x11.c, you'll need libX11.a too.

# for regular vile, use these:
SCREEN = vmsvt
LIBS =
TARGET = vile.exe
SCRDEF = "VMSVT","scrn_chosen"

# for building the X version, xvile, use these:
#SCREEN = x11
#LIBS = #-lX11
#TARGET = xvile.exe
#SCRDEF = "X11","scrn_chosen"

# if you want the help file (vile.hlp) to go somewhere other than your $PATH
#  or one of the hard-code paths in epath.h  (it goes to the same place vile
#  does by default)
HELP_LOC=

LINKFLAGS = /MAP=$(MMS$TARGET_NAME)/CROSS_REFERENCE/EXEC=$(MMS$TARGET_NAME).EXE

INCS = []

# All of the makefiles which should be preserved
MAKFILES = makefile make.ini descrip.mms
MKTBLS = mktbls.EXE

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
CSRCfh = fences.c file.c filec.c fileio.c finderr.c glob.c globals.c history.c hp110.c hp150.c
CSRCim = ibmpc.c input.c insert.c isearch.c line.c main.c map.c modes.c mktbls.c
CSRCnr = npopen.c opers.c oneliner.c path.c random.c regexp.c region.c
CSRCst = search.c spawn.c st520.c tags.c tbuff.c tcap.c termio.c tipc.c tmp.c
CSRCuw = undo.c version.c vmalloc.c vms2unix.c vmspipe.c vmsvt.c vt52.c window.c word.c wordmov.c
CSRCxz = x11.c z309.c z_ibmpc.c

CSRC = $(CSRCac) $(CSRCde) $(CSRCfh) $(CSRCim) $(CSRCnr) \
	$(CSRCst) $(CSRCuw) $(CSRCxz)

# non-C source code
OTHERSRC = z100bios.asm

# text and data files
TEXTFILES = README CHANGES cmdtbl modetbl vile.hlp buglist revlist \
	README.X11

ALLSRC = $(CSRC) $(OTHERSRC)

EVERYTHING = $(ALLTOOLS) $(ALLHDRS) $(ALLSRC) $(TEXTFILES) $(SHORTSTUFF)

SRC =	main.c \
	$(SCREEN).c \
	basic.c \
	bind.c \
	buffer.c \
	crypt.c \
	csrch.c \
	display.c \
	eval.c \
	exec.c \
	externs.c \
	fences.c \
	file.c \
	filec.c \
	fileio.c \
	finderr.c \
	glob.c \
	globals.c \
	history.c \
	input.c \
	insert.c \
	isearch.c \
	line.c \
	map.c \
	modes.c \
	npopen.c \
	oneliner.c \
	opers.c \
	path.c \
	random.c \
	regexp.c \
	region.c \
	search.c \
	spawn.c \
	tags.c \
	tbuff.c \
	termio.c \
	tmp.c \
	undo.c \
	version.c \
	vmalloc.c \
	vms2unix.c \
	vmspipe.c \
	window.c \
	word.c \
	wordmov.c

OBJ =	main.obj,\
	$(SCREEN).obj,\
	basic.obj,\
	bind.obj,\
	buffer.obj,\
	crypt.obj,\
	csrch.obj,\
	display.obj,\
	eval.obj,\
	exec.obj,\
	externs.obj,\
	fences.obj,\
	file.obj,\
	filec.obj,\
	fileio.obj,\
	finderr.obj,\
	glob.obj, \
	globals.obj,\
	history.obj,\
	input.obj,\
	insert.obj,\
	isearch.obj,\
	line.obj,\
	map.obj, \
	modes.obj,\
	npopen.obj,\
	oneliner.obj,\
	opers.obj,\
	path.obj,\
	random.obj,\
	regexp.obj,\
	region.obj,\
	search.obj,\
	spawn.obj,\
	tags.obj,\
	tbuff.obj,\
	termio.obj,\
	tmp.obj,\
	undo.obj,\
	version.obj, \
	vmalloc.obj,\
	vms2unix.obj,\
	vmspipe.obj,\
	window.obj,\
	word.obj,\
	wordmov.obj

all :	$(TARGET)
	@ WRITE SYS$OUTPUT "** made $@"

nebind.h \
nefunc.h \
nename.h :	cmdtbl $(MKTBLS)
	MKTBLS cmdtbl

nevars.h \
nemode.h :	modetbl $(MKTBLS)
	MKTBLS modetbl

# install to DESTDIR1 if it's writable, else DESTDIR2
install :
	@ WRITE SYS$ERROR "** no rule for $@"
	
clean :
	@- if f$search("*.com") .nes. "" then delete *.com;*
	@- if f$search("*.obj") .nes. "" then delete *.obj;*
	@- if f$search("*.bak") .nes. "" then delete *.bak;*
	@- if f$search("*.lis") .nes. "" then delete *.lis;*
	@- if f$search("*.log") .nes. "" then delete *.log;*
	@- if f$search("*.map") .nes. "" then delete *.map;*
	@- if f$search("ne*.h") .nes. "" then delete ne*.h;
	@- if f$search("$(MKTBLS)") .nes. "" then delete $(MKTBLS);

clobber : clean
	@- if f$search("*.exe") .nes. "" then delete *.exe;*

$(OBJ) : estruct.h nemode.h edef.h proto.h

main.obj :	nevars.h glob.h
bind.obj :	epath.h
eval.obj :	glob.h
filec.obj :	dirstuff.h
eval.obj :	nevars.h
glob.obj :	dirstuff.h glob.h
externs.obj :	nebind.h nename.h nefunc.h
random.obj :	glob.h
vmalloc.obj :	nevars.h
vms2unix.obj :	dirstuff.h

.first :
	@ define/nolog SYS SYS$LIBRARY		! fix includes to <sys/...>
	@ MKTBLS :== $SYS$DISK:'F$DIRECTORY()$(MKTBLS)	! make a foreign command

.last :
	@- if f$search("*.dia") .nes. "" then delete *.dia;*
	@- if f$search("*.lis") .nes. "" then purge *.lis
	@- if f$search("*.obj") .nes. "" then purge *.obj
	@- if f$search("*.map") .nes. "" then purge *.map
	@- if f$search("*.exe") .nes. "" then purge *.exe
	@- if f$search("*.log") .nes. "" then purge *.log

# used /G_FLOAT with vaxcrtlg/share in vms_link.opt
# can also use /Listing, /Show=All
CFLAGS =-
	/Diagnostics /Debug /Define=("os_chosen",$(SCRDEF)) -
	/Object=$@ /Include=($(INCS))

.C.OBJ :
	$(CC) $(CFLAGS) $(MMS$SOURCE)
	@- delete $(MMS$TARGET_NAME).dia;*

$(MKTBLS) : mktbls.obj
	$(LINK) $(LINKFLAGS) mktbls.obj,SYS$LIBRARY:VAXCRTL/LIB

$(TARGET) : $(OBJ), vms_link.opt, descrip.mms
	$(LINK) $(LINKFLAGS) main.obj, $(SCREEN).obj, vms_link/opt

# Runs VILE from the current directory (used for testing)
vile.com :
	@- if "''f$search("$@")'" .nes. "" then delete $@;*
	@- copy nl: $@
	@ open/append  test_script $@
	@ write test_script "$ vile :== $""sys$disk:''f$directory()'vile.exe""
	@ write test_script "$ define/user_mode sys$input  sys$command"
	@ write test_script "$ define/user_mode sys$output sys$command"
	@ write test_script "$ vile 'p1 'p2 'p3 'p4 'p5 'p6 'p7 'p8"
	@ close test_script
	@ write sys$output "** made $@"

# Runs XVILE from the current directory (used for testing)
xvile.com :
	@- if "''f$search("$@")'" .nes. "" then delete $@;*
	@- copy nl: $@
	@ open/append  test_script $@
	@ write test_script "$ xvile :== $""sys$disk:''f$directory()'xvile.exe""
	@ write test_script "$ define/user_mode sys$input  sys$command"
	@ write test_script "$ define/user_mode sys$output sys$command"
	@ write test_script "$ xvile 'p1 'p2 'p3 'p4 'p5 'p6 'p7 'p8"
	@ close test_script
	@ write sys$output "** made $@"

# $Log: descrip.mms,v $
# Revision 1.1  1994/02/01 03:29:14  jkh
# Initial revision
#
# Revision 1.10  1993/09/03  09:11:54  pgf
# tom's 3.60 changes
#
# Revision 1.9  1993/08/05  14:29:12  pgf
# tom's 3.57 changes
#
# Revision 1.8  1993/07/15  10:37:58  pgf
# see 3.55 CHANGES
#
# Revision 1.7  1993/07/06  16:53:50  pgf
# added map.c/map.obj
#
# Revision 1.6  1993/06/18  15:57:06  pgf
# tom's 3.49 changes
#
# Revision 1.5  1993/04/28  14:34:11  pgf
# see CHANGES, 3.44 (tom)
#
# Revision 1.4  1993/04/20  12:18:32  pgf
# see tom's 3.43 CHANGES
#
# Revision 1.3  1993/04/01  13:07:50  pgf
# see tom's 3.40 CHANGES
#
# Revision 1.2  1993/03/25  19:50:58  pgf
# see 3.39 section of CHANGES
#
# Revision 1.1  1993/03/17  09:50:19  pgf
# Initial revision
#
#
