#
# makefile for Watcom C using Watcom MAKE 
# based on the original makefile of vile 3.46 (see the original makefile)
# T.DANG (dang@cogit.ign.fr)
#
# $Log: makefile.wat,v $
# Revision 1.1  1994/02/01 03:29:32  jkh
# Initial revision
#
# Revision 1.3  1993/09/03  09:11:54  pgf
# tom's 3.60 changes
#
# Revision 1.2  1993/07/27  19:20:00  pgf
# changed default flags to /ols, to optimize by default
#
# Revision 1.1  1993/07/09  14:01:14  pgf
# new clean target, added map.c/map.obj, added /p to compile lines
#
# Revision 1.0  1993/06/25  14:41:16  pgf
# Initial revision
#
#
#define PVGA for Paradise VGA (because there are some problems with this card)
#To fix it, use /dPVGA=1
#CFLAGS= /dIBMPC=1 /dscrn_chosen=1 /dPVGA=1 
CFLAGS= /dIBMPC=1 /dscrn_chosen=1

SCREEN= ibmpc 

# these are normal editable headers
HDRS = estruct.h epath.h edef.h proto.h dirstuff.h glob.h

# these headers are built by the mktbls program from the information in cmdtbl
# and in modetbl
BUILTHDRS = nebind.h nefunc.h nemode.h nename.h nevars.h

SRC = 	main.c $(SCREEN).c basic.c bind.c buffer.c crypt.c &
	csrch.c display.c eval.c exec.c externs.c fences.c file.c filec.c &
	fileio.c finderr.c glob.c globals.c history.c input.c insert.c isearch.c &
	line.c modes.c npopen.c oneliner.c opers.c path.c random.c regexp.c &
	region.c search.c spawn.c tags.c tbuff.c termio.c tmp.c undo.c &
	version.c vmalloc.c window.c word.c wordmov.c map.c

OBJ = 	main.obj $(SCREEN).obj basic.obj bind.obj buffer.obj crypt.obj &
      	csrch.obj display.obj eval.obj exec.obj externs.obj fences.obj file.obj filec.obj &
	fileio.obj finderr.obj glob.obj globals.obj history.obj input.obj insert.obj isearch.obj &
	line.obj modes.obj npopen.obj oneliner.obj opers.obj path.obj random.obj regexp.obj &
	region.obj search.obj spawn.obj tags.obj tbuff.obj termio.obj tmp.obj undo.obj &
	version.obj vmalloc.obj window.obj word.obj wordmov.obj map.obj


vile.exe: $(BUILTHDRS) $(OBJ) vile.lnk
	wlink @vile 

vile.lnk: $(OBJ)
	echo DEBUG ALL >$^@
        echo NAME vile >>$^@
	for %i in ($(OBJ)) do echo FILE %i >>$^@

.c.obj:	estruct.h nemode.h edef.h proto.h 
#	wcl386/p $[* /c /d2 $(CFLAGS) 
	wcl386/p $[* /c /ols $(CFLAGS) 

nebind.h &
nefunc.h &
nename.h &
nevars.h :	cmdtbl MKTBLS.EXE
	MKTBLS.EXE cmdtbl

nemode.h:	modetbl MKTBLS.EXE
	MKTBLS.EXE modetbl

MKTBLS.EXE:  mktbls.c
	wcl386/p mktbls.c
	del mktbls.obj
clean:
	del *.obj
	del nebind.h
	del nefunc.h
	del nename.h
	del nevars.h
	del nemode.h
	del MKTBLS.EXE
