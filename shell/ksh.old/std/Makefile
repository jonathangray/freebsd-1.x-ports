#
# Makefile for the compatibility libraries
#
# Makefile,v 1.3 1993/05/21 06:12:48 cgd Exp
#

SHELL = /bin/sh
MAKE  = make

#CONFIG = -D_SYSV
CONFIG = -D_BSD

LN = ln

libs:	h libstdc.a libposix.a

h:
#	mkdir h
	( cd stdc ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' clean link )
	( cd posix  ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' clean link )
	( cd ../sh ;  $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' 'LN=$(LN)' link )

libstdc.a: FORCED
	( cd stdc ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' all )

libposix.a: FORCED
	( cd posix ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' all )

clean:
	-rm -f *.out
	-rm -rf h/[a-zA-BD-Z]*
	-rm -rf *.a
	( cd stdc ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )
	( cd posix  ; $(MAKE) 'CC=$(CC)' 'CONFIG=$(CONFIG)' $@ )

FORCED:
