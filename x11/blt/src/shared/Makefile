# Generated automatically from Makefile.in by configure.
# ------------------------------------------------------------------------
# 	Makefile for shared version of BLT library
# ------------------------------------------------------------------------

MAJOR_VERSION	= 1
MINOR_VERSION	= 7
LIBNAME		= libBLT

# ------------------------------------------------------------------------
# 	C Compiler options 
# ------------------------------------------------------------------------

CC 		= cc
CFLAGS 		= -O
DEFINES 	=  -DBLT_LIBRARY=\"$(BLT_LIBRARY)\"
SHLIB_CCFLAGS 	= -pic
SHLIB_LDFLAGS 	= -assert pure-text
SHLIB_LD 	= ld
SHLIB_SUFFIX 	= .so.$(MAJOR_VERSION).$(MINOR_VERSION)
SHARED_LIBRARY 	= $(LIBNAME)$(SHLIB_SUFFIX)
TOPDIR 		= /opt/cellar/deli/blt
LIBDIR 		= /opt/cellar/deli/blt/lib
BLT_LIBRARY 	= /opt/cellar/deli/blt/library

# ------------------------------------------------------------------------
# 	Directories containing Tcl and Tk include files and libraries
# ------------------------------------------------------------------------

TCL_INCLUDE 	= -I/opt/cellar/tcl/tcl7.3/include
TK_INCLUDE 	= -I/opt/cellar/tk/tk3.6/include
X_INCLUDE 	= -I/usr/X11/include

# ------------------------------------------------------------------------
# 	Don't edit anything beyond this point
# ------------------------------------------------------------------------

OBJS 		= bltGraph.o bltGrAxis.o bltGrBar.o bltGrElem.o \
		  bltGrHairs.o bltGrLegd.o bltGrLine.o bltGrMisc.o \
		  bltGrPS.o bltGrTag.o  \
        	  bltCutbuffer.o bltBell.o \
        	  bltInit.o bltBgexec.o bltBitmap.o bltBusy.o \
		  bltDebug.o bltDragDrop.o bltHtext.o bltList.o \
	          bltTable.o bltUtil.o bltWatch.o bltWin.o 

INCLUDES 	= -I. $(TK_INCLUDE) $(TCL_INCLUDE) $(X_INCLUDE)
CC_SWITCHES	= $(SHLIB_CCFLAGS) $(CFLAGS) $(DEFINES) $(INCLUDES)
INSTALL 	= /bin/install -c
INSTALL_DATA 	= $(INSTALL) -m 644
SHELL 		= /bin/sh
RM 		= rm -f
srcdir 		= ./..

all: $(SHARED_LIBRARY)

$(SHARED_LIBRARY):  $(OBJS)
	$(RM) $@
	$(SHLIB_LD) $(SHLIB_LDFLAGS) -o $@ $(OBJS)

install: install-dirs
	-if test -f $(SHARED_LIBRARY) ; then \
	    $(INSTALL_DATA) $(SHARED_LIBRARY) $(LIBDIR) ; \
	fi

install-dirs:
	@for i in $(TOPDIR) $(LIBDIR) ; do \
	    if test ! -d $$i ; then \
		mkdir $$i ; \
	    fi ; \
	done

clean:
	$(RM) $(OBJS) $(LIBNAME)* *\~ "#"* 

# ------------------------------------------------------------------------
# 	in lieu of viewpath-ing...
#
bltBell.o: 	$(srcdir)/bltBell.c 
	$(CC) -c $(CC_SWITCHES) $?
bltBgexec.o: 	$(srcdir)/bltBgexec.c
	$(CC) -c $(CC_SWITCHES) $?
bltBitmap.o: 	$(srcdir)/bltBitmap.c
	$(CC) -c $(CC_SWITCHES) $?
bltBusy.o: 	$(srcdir)/bltBusy.c
	$(CC) -c $(CC_SWITCHES) $?
bltCutbuffer.o: $(srcdir)/bltCutbuffer.c
	$(CC) -c $(CC_SWITCHES) $?
bltDragDrop.o: $(srcdir)/bltDragDrop.c
	$(CC) -c $(CC_SWITCHES) $?
bltDebug.o: $(srcdir)/bltDebug.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrAxis.o:	$(srcdir)/bltGrAxis.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrBar.o:	$(srcdir)/bltGrBar.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrElem.o:	$(srcdir)/bltGrElem.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrHairs.o:	$(srcdir)/bltGrHairs.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrLegd.o: 	$(srcdir)/bltGrLegd.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrLine.o:	$(srcdir)/bltGrLine.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrMisc.o: 	$(srcdir)/bltGrMisc.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrPS.o: 	$(srcdir)/bltGrPS.c
	$(CC) -c $(CC_SWITCHES) $?
bltGrTag.o: 	$(srcdir)/bltGrTag.c
	$(CC) -c $(CC_SWITCHES) $?
bltGraph.o:	$(srcdir)/bltGraph.c
	$(CC) -c $(CC_SWITCHES) $?
bltHtext.o: 	$(srcdir)/bltHtext.c
	$(CC) -c $(CC_SWITCHES) $?
bltInit.o: 	$(srcdir)/bltInit.c       
	$(CC) -c $(CC_SWITCHES) $?
bltList.o: 	$(srcdir)/bltList.c
	$(CC) -c $(CC_SWITCHES) $?
bltTable.o: 	$(srcdir)/bltTable.c
	$(CC) -c $(CC_SWITCHES) $?
bltUtil.o: 	$(srcdir)/bltUtil.c
	$(CC) -c $(CC_SWITCHES) $?
bltWatch.o: $(srcdir)/bltWatch.c
	$(CC) -c $(CC_SWITCHES) $?
bltWin.o:       $(srcdir)/bltWin.c
	$(CC) -c $(CC_SWITCHES) $?
