#
#       Compilation and loading instructions for IDL servers
#
#       @(#)makefile	2.1 93/03/07 23:55:55
#
#
IDLHOME         = ..

#
#
#
LDFLAGS         = -L/users/X11R5/lib
XTLIBS          =  $(LDFLAGS) -lXaw -lXmu -lXt -lXext -lX11 $(LGCC)
IDLLIBS         = -L$(IDLHOME)/Xidl -lidl
CURSESLIBS      = -lcurses -ltermcap
INCLUDE         = -I$(IDLHOME)/Xidl

#
#       We don't actually need gcc, but the error reports are better
#
CC              = gcc
LGCC            = -lgcc
CFLAGS          = -traditional-cpp -I/users/X11R5/include

#
#               Compilation rules
#
.SUFFIXES:      .idl .sml
%.c:            %.idl   ;  idl $*
%.sml:          %.idl   ;  idl $*

#
#               Example X/idl server
#
idlx:           idlx.o $(IDLHOME)/Xidl/libidl.a;
		$(CC) $(CFLAGS) -o idlx idlx.o $(IDLLIBS) $(XTLIBS) $(LGCC)

idlx.o:         idlx.c ; $(CC) $(CFLAGS) $(INCLUDE) -c idlx.c

#
#               Curses/idl server
#
idlcurses:      idlcurses.o $(IDLHOME)/Xidl/libidl.a;
		$(CC) $(CFLAGS) -o idlcurses idlcurses.o $(IDLLIBS) $(CURSESLIBS) $(LGCC)

idlcurses.o:    idlcurses.c ; $(CC) $(CFLAGS) $(INCLUDE) -c idlcurses.c

#
#               Trivial non-X/idl server
#
idl0:           idl0.c;
		$(CC) $(CFLAGS) $(INCLUDE) -o idl0 idl0.c $(IDLLIBS)
#
#
#
SOURCES         = idl0.idl idl0.run.sml                 \
		  idlx.idl idlx.run.sml idlx.canvas.sml \
		  idlcurses.idl idlcurses.run.sml
DISTRIB:        ; sccs get $(SOURCES)
#
#
#
