#
#		Makefile for tcx
#
#	Author : Stewart Forster, University Of Melbourne, 25/31993
#

# C compiler of your choice.  Should be ansi'ish
CC = cc			# IRIX, ULTRIX, AIX
#CC = gcc		# SUNOS


# Cflags of choice.
CFLAGS = -s -O	-Wall	# Most OS's
#CFLAGS = -s -O6 -fomit-frame-pointer -Wall	# Linux


# Any libraries
#LIBS = -lmalloc -lc_s	# IRIX
LIBS =			# SUNOS, ULTRIX, AIX


all:	tcx untcx

tcx:	tcx.c config.h
	$(CC) $(CFLAGS) -o tcx tcx.c $(LIBS)

untcx:	untcx.c config.h
	$(CC) $(CFLAGS) -o untcx untcx.c $(LIBS)

shar:
	shar -c -l 45 -o tcx README VERSION COPYING Makefile tcx.1 untcx.1 tcx.c config.h untcx.c

clean:
	/bin/rm -f *.o tcx untcx

install: tcx untcx
	install -c -s -o bin -g bin -m 555 tcx /usr/local/bin/tcx
	install -c -s -o root -g bin -m 4711 untcx /usr/local/bin/untcx
	install -c -o bin -g bin -m 444 tcx.1 /usr/local/man/man1/tcx.1
	install -c -o bin -g bin -m 444 untcx.1 /usr/local/man/man1/untcx.1
	mkdir /var/exec


pkg:	pkg_src pkg_bin

pkg_src: clean pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S tcx_src

pkg_bin: install pkg/+DESC_B pkg/+ONELINE_B pkg/+PLIST_B 
	pkg_create -c pkg/+ONELINE_B -d pkg/+DESC_B -f pkg/+PLIST_B tcx_bin
