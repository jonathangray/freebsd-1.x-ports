# Makefile for "top", a top 10 process display for Unix
#
# This makefile is for top, version 3
#
# Written by William LeFebvre, Northwestern University
#			(formerly Rice University)

# DO NOT EDIT "Makefile"!!!!  Make changes to "Makefile.X" and rerun
# Configure.

# Executables (these should be obvious):

SHELL   = %shell%
CC      = %cc%
AWK     = %awk%
INSTALL = %install%

# installation information:
#	OWNER	- name (or uid) for the installed executable's owner
#	GROUP	- group name (or gid) for the installed executable's group
#	MODE	- mode for the installed executable (should start with a 0)
#	BINDIR	- directory where the executable should live
#	MANDIR	- directory where the manual page should live
#	MAN	- troff macros for manual pages
#	SIGNAL	- <signal.h> or <sys/signal.h>; the one with signal definitions
#	TROFF	- most appropriate troff command

OWNER  = %owner%
GROUP  = %group%
MODE   = %mode%
BINDIR = %bindir%
MANDIR = %mandir%
MANEXT = %manext%
SIGNAL = %signal%

# Values for the two defaults in "top":
#	TOPN	- default number of processes to display
#	DELAY	- default delay between updates
#
# set TOPN to -1 to indicate infinity (so that top will display as many
# as the screen will hold).

TOPN = %topn%
DELAY = %delay%

TARFILES = README README.sun4m Changes Configure Porting \
	   Makefile.X Make.desc.X getans \
	   top.c commands.c display.c screen.c username.c \
	   utils.c version.c getopt.c \
	   boolean.h display.h layout.h loadavg.h screen.h \
	   machine.h patchlevel.h top.h top.local.H os.h \
	   sigconv.awk top.X m-template suntop \
	   %modules% \
	   %manmodules%
CFILES = top.c commands.c display.c screen.c username.c \
	 utils.c version.c getopt.c machine.c
OBJS = top.o commands.o display.o screen.o username.o \
       utils.o version.o getopt.o machine.o

CDEFS = %cdefs%
LIBS = %libs%

CFLAGS = $(CDEFS)
LINTFLAGS = -x $(CDEFS)

all: Makefile top.local.h top

Makefile: Makefile.X
	@echo 'You need to run the script "Configure" before running "make".'
	exit 10

top.local.h: top.local.H
	@echo 'You need to run the script "Configure" before running "make".'
	exit 10

top: $(OBJS)
	$(CC) -o top $(OBJS) -ltermcap -lm $(LIBS)

lint: sigdesc.h
	$(LINT) $(LINTFLAGS) $(CFILES)

# include file dependencies
top.o: boolean.h display.h screen.h top.h top.local.h machine.h
commands.o: sigdesc.h
display.o: boolean.h display.h layout.h screen.h top.h top.local.h
machine.o: top.h machine.h
screen.o: boolean.h screen.h
utils.o: top.h
version.o: top.h patchlevel.h

# automatically built include file
sigdesc.h: sigconv.awk $(SIGNAL)
	$(AWK) -f sigconv.awk $(SIGNAL) >sigdesc.h

tar:
	rm -f top.tar
	tar cvf top.tar $(TARFILES)

shar:
	rm -f top.shar*
	packmail 62000 top.shar $(TARFILES)

clean:
	rm -f *.o top core core.* sigdesc.h

veryclean: clean
	rm -f *.desc .defaults top.tar Makefile SYNOPSIS

install: top top.1
	$(INSTALL) -s -o $(OWNER) -m $(MODE) -g $(GROUP) top $(BINDIR)
	$(INSTALL) -c top.1 $(MANDIR)/top.$(MANEXT)
