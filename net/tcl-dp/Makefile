#
# This Makefile creates the Tcl-DP extension library to Tcl/Tk.  
# Edit the following variables to reflect your environment.
#
# TCL_INC 	-- directory location of Tcl 6.5 include files  
# TCL_LIB 	-- directory location of Tcl 6.5 library (libtcl.a)
#
# TK_INC 	-- directory location of Tk 3.0 include files
# TK_LIB	-- directory location of Tk 3.0 library (libtk.a)
#
# INCLUDE_STD	-- directory of standard include files (like stdio.h)
# INCLUDE_X	-- directory of X include files (like X11/Xlib.h)
#
# LIB_STD	-- directory location of standard libraries (like libc.a)
# LIB_X 	-- directory location of X libraries (like libX11.a)
#
# ENDIAN should be set to either LITTLE_ENDIAN or BIG_ENDIAN,
# depending on your machine architecture type.  For example,
# Sun SPARCstations (SPARC CPU) are BIG_ENDIAN;
# DEC DECstations (MIPS CPU) are LITTLE_ENDIAN;
#

CC = gcc

TCL_INC = /usr/ports/tcl
TCL_LIB = /usr/local/lib

TK_INC	= /usr/ports/tk
TK_LIB	= /usr/local/lib

INCLUDE_STD 	= /usr/include
INCLUDE_X	= /usr/X386/include

LIB_STD	= /usr/lib
LIB_X	= /usr/X386/lib

ENDIAN	= BIG_ENDIAN

######################################################################

LFLAGS	= -L$(TCL_LIB) -L$(TK_LIB) -L$(LIB_X) -L$(LIB_STD)

CFLAGS		= -O -I$(TCL_INC) -I$(TK_INC) \
	-I$(INCLUDE_STD) -I$(INCLUDE_X) -D${ENDIAN}
LINTFLAGS 	= -u -I$(TCL_INC) -I$(TK_INC) \
	-I$(INCLUDE_STD) -I$(INCLUDE_X) -D${ENDIAN}

######################################################################

OBJS 	= networkInit.o network.o address.o rpc.o
SRCS 	= networkInit.c network.c address.c rpc.c

default: dpwish

libtnetwork.a: $(OBJS)
	rm -f libtnetwork.a
	ar cr libtnetwork.a $(OBJS)
	ranlib libtnetwork.a

libtutil.a: util.o
	rm -f libtutil.a
	ar cr libtutil.a util.o
	ranlib libtutil.a

dpwish: check main.o libtnetwork.a libtutil.a
	$(CC) $(LFLAGS) main.o libtnetwork.a libtutil.a \
	-ltcl -ltk -lX11 -lm -o dpwish

check:
	@if [ ! -f /usr/local/lib/libtk.a ]; then \
		echo "You must first install tk and tcl before doing this package!" ; \
		exit 1; \
	fi

clean:
	rm -f $(OBJS) libtnetwork.a libtutil.a dpwish main.o util.o

install:
	install -c libtnetwork.a /usr/local/lib && \
	 ranlib /usr/local/lib/libtnetwork.a
	install -c libtutil.a /usr/local/lib && \
	 ranlib /usr/local/lib/libtutil.a
	install -c -s dpwish /usr/local/bin
	@if [ ! -d /usr/local/man/man3 ]; then \
		mkdir /usr/local/man/man3 ; \
	fi
	install -c man/man3/* /usr/local/man/man3
	@if [ ! -d /usr/local/man/manl ]; then \
		mkdir /usr/local/man/manl ; \
	fi
	install -c man/manl/* /usr/local/man/manl

#
# The following is lint magic.
#

lint:	lintout

lintout:	${SRCS}
	lint ${LINTFLAGS} ${SRCS} | \
	egrep -v 'possible pointer alignment problem|returns value which is.*ignored' > lintout

.SUFFIXES: .lint
.c.lint:
	lint ${LINTFLAGS} $< | \
	egrep -v 'possible pointer alignment problem|returns value which is.*ignored'

