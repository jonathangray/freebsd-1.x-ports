# Generated automatically from Makefile.in by configure.
# ------------------------------------------------------------------------
# 	Makefile for normal version of BLT library
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 	C Compiler options 
# ------------------------------------------------------------------------

CC           	= cc
CFLAGS  	= -O
DEFINES      	=  -DBLT_LIBRARY=\"$(BLT_LIBRARY)\"
TOPDIR      	= /opt/cellar/deli/blt
LIBDIR      	= /opt/cellar/deli/blt/lib
BLT_LIBRARY  	= /opt/cellar/deli/blt/library

# ------------------------------------------------------------------------
# 	Directories containing Tcl and Tk include files and libraries
# ------------------------------------------------------------------------

TCL_INCLUDE   	= -I/opt/cellar/tcl/tcl7.3/include
TK_INCLUDE    	= -I/opt/cellar/tk/tk3.6/include
X_INCLUDE     	= -I/usr/X11/include

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

SRCS 		= bltGraph.c bltGrAxis.c bltGrBar.c bltGrElem.c \
		  bltGrHairs.c bltGrLegd.c bltGrLine.c bltGrMisc.c \
		  bltGrPS.c bltGrTag.c  \
        	  bltCutbuffer.c bltBell.c \
        	  bltInit.c bltBgexec.c bltBitmap.c bltBusy.c \
		  bltDebug.c bltDragDrop.c bltHtext.c bltList.c \
	          bltTable.c bltUtil.c bltWatch.c bltWin.c 

libname  	= libBLT.a
INCLUDES	= -I. $(TK_INCLUDE) $(TCL_INCLUDE) $(X_INCLUDE)
CC_SWITCHES	= $(CFLAGS) $(DEFINES) $(INCLUDES)
INSTALL  	= /bin/install -c
INSTALL_DATA  	= $(INSTALL) -m 644
RANLIB   	= ranlib
SHELL    	= /bin/sh
AR       	= ar rc
RM       	= rm -f
LINT		= lint
LINTFLAGS	= -axhbn
srcdir 		= .
VPATH		= .

all: $(libname)

$(libname):  $(OBJS)
	$(RM) $@
	$(AR) $@ $(OBJS)
	$(RANLIB) $@

install: $(libname) install-dirs
	$(INSTALL_DATA) $(libname) $(LIBDIR)
	$(RANLIB) $(LIBDIR)/$(libname)

install-dirs:
	@for i in $(TOPDIR) $(LIBDIR) ; do \
	    if test ! -d $$i ; then \
		mkdir $$i ; \
	    fi ; \
        done

clean:
	$(RM) $(OBJS) $(libname) *\~ "#"*

lint:
	$(LINT) $(LINTFLAGS) $(DEFINES) $(INCLUDES) $(SRCS)

.c.o:
	$(CC) -c $(CC_SWITCHES) $<

