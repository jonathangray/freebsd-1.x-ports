#
# This Makefile creates and installs the swish program
# and various services server Tcl/Tk scripts.  The swish 
# program is used by all services server scripts. 
#
# Edit the following variables to reflect your environment.
#
# TCL_INC 	-- directory location of Tcl 6.5 include files  
# TCL_LIB 	-- directory location of Tcl 6.5 library (libtcl.a)
#
# TK_INC 	-- directory location of Tk 3.0 include files
# TK_LIB	-- directory location of Tk 3.0 library (libtk.a)
#
# DP_INC	-- directory location of Tcl-DP include files
# DP_LIB	-- directory location of Tcl-DP libraries
#
# INCLUDE_STD	-- directory of standard include files (like stdio.h)
# INCLUDE_X	-- directory of X include files (like X11/Xlib.h)
#
# LIB_X 	-- directory location of X library (libX11.a)
# LIB_STD	-- directory location of standard libraries
#
#

TCL_INC = /n/gumby/hp-users/tcl-dp/tcl+tk/tk3.2/tcl
TCL_LIB = /n/gumby/hp-users/tcl-dp/tcl+tk/tk3.2/tcl

TK_INC	= /n/gumby/hp-users/tcl-dp/tcl+tk/tk3.2
TK_LIB	= /n/gumby/hp-users/tcl-dp/tcl+tk/tk3.2

DP_INC	= ../..
DP_LIB	= ../..

INCLUDE_STD	= /usr/include
INCLUDE_X	= /usr/include

LIB_STD		= /usr/lib
LIB_X		= /usr/sww/X11/lib

#
# When used as part of the Berkeley Plauteau continuous media
# toolkit, the following variable BASE refers to the root 
# directory of that software.  BASE is used in the 
# following INSTALL_DIR variable.
#

BASE = /n/picasso/project/mm/cms

#
# INSTALL_DIR	-- absolute path of directory location where you want 
#		-- executables (scripts and swish) installed.
#

INSTALL_DIR	= $(BASE)/bin/$(ARCH)
INSTALL_DIR	= /n/gumby/hp-users/tcl-dp/test/bin

#
# If you are using this software as part of the Berkeley Plateau
# continous media toolkit, set the following CM flag 
# to BERKELEY_PLATEAU.  Otherwise set CM to nothing.
#

CM 	= BERKELEY_PLATEAU
CM	= 

#
# If you are using this software as part of the Berkeley Plateau
# continous media toolkit, use the LIBS line that has tks and tclx.  
# Otherwise use the LIBS line that has tk and tcl.
#

LIBS 	= -ltnetwork -ltutil -ltks -ltclx -lX11 -ll -lm -lc
LIBS 	= -ltnetwork -ltutil -ltk -ltcl -lX11 -ll -lm -lc

#########################################################################

CFLAGS	= -g -I$(DP_INC) -I$(TCL_INC) -I$(TK_INC) -D$(CM) \
	-I$(INCLUDE_X) -I$(INCLUDE_STD)

LFLAGS	=    -L$(DP_LIB) -L$(TCL_LIB) -L$(TK_LIB) \
	-L$(LIB_X) -L$(LIB_STD)

CC	= cc

#########################################################################

default: swish

install: install-swish install-scripts

install-swish: swish
	rm -f $(INSTALL_DIR)/swish
	cp swish $(INSTALL_DIR)/swish

install-scripts:
	swish -f install-scripts.tcl $(INSTALL_DIR)

swish: swish.o
	$(CC) $(LFLAGS) swish.o $(LIBS) -o swish

clean:
	rm -f swish swish.o
	rm -f killservice lservices rmservice serviced

