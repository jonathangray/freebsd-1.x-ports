#
#       Makefile for the idl/xidl library
#
#       @(#)Makefile	2.1 93/03/07 23:56:23
#
LIBDIR          = /usr/local/lib
INCDIR          = /usr/include/X11/Xidl

#
#
#
PROGS   = SimpleCanvas.c idlxt.c idlbase.c
INCS    = idlbase.h SimpleCanvasP.h SimpleCanvas.h
OBJS    = ${PROGS:%.c=%.o}
CC      = gcc
#
#
#
CFLAGS  =

libidl.a:       $(OBJS) ;
				rm -f  libidl.a
				ar r   libidl.a $(OBJS)
				ranlib libidl.a

install:        libidl.a;       cp -p libidl.a $(LIBDIR)
				cp -p $(INCS) $(INCDIR)

clean:                  ;       rm -f *.o *.a core

#
#
#
SML=            idlbase.sig.sml idlbase.sml
DISTRIB:        ; sccs get $(PROGS) $(INCS) $(SML)
#
#
#

