#
# Toplevel FreeBSD Makefile for DGD, Dworkin's Game Driver
#
HOST=	BSD386
DEFINES=-D$(HOST)
BASEDIR=/usr/local/lib/dgd
MUDLIB=2.4.5
CC=gcc
LD=$(CC)
CC_OPT=$(DEFINES) -O2 -fstrength-reduce -fomit-frame-pointer
LD_OPT=-s
LIBS=	-lcrypt
DMAKE=	make
PK=	pkg.bin
PKG=	$(.CURDIR)/$(PK)
OBJ=    alloc.o error.o hash.o swap.o str.o array.o object.o data.o path.o\
ed.o comm.o call_out.o interpret.o config.o dgd.o

all:
	@cd src; $(DMAKE)

package:	$(PKG)/+DESC $(PKG)/+ONELINE $(PKG)/+PLIST $(PKG)/+REQ
	@cd $(PK); for dir in comp lex ed kfun lpc host; do if [ ! -d $$dir ]; then mkdir $$dir; fi; done
	cd src; MAKEOBJDIR=$(PKG) $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CC_OPT)' 'CFLAGS= -I$(.CURDIR)/src -I$(.CURDIR)/src/comp -I$(.CURDIR)/src/lex -I$(.CURDIR)/src/ed -I$(.CURDIR)/src/kfun $(CC_OPT)' $(OBJ)
	cd src/comp; MAKEOBJDIR=$(PKG)/comp $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CC_OPT)' 'CFLAGS= -I$(.CURDIR)/src/comp -I$(.CURDIR)/src -I$(.CURDIR)/src/lex -I$(.CURDIR)/src/kfun $(CC_OPT)' dgd
	cd src/lex; MAKEOBJDIR=$(PKG)/lex $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CC_OPT)' 'CFLAGS= -I$(.CURDIR)/src/lex -I$(.CURDIR)/src -I$(.CURDIR)/src/comp $(CC_OPT)' dgd
	cd src/ed; MAKEOBJDIR=$(PKG)/ed $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CC_OPT)' 'CFLAGS= -I$(.CURDIR)/src/ed -I$(.CURDIR)/src -I$(.CURDIR)/src/lex -I$(.CURDIR)/src/kfun $(CC_OPT)' dgd
	cd src/kfun; MAKEOBJDIR=$(PKG)/kfun $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CC_OPT)' 'CFLAGS= -I$(.CURDIR)/src/kfun -I$(.CURDIR)/src -I$(.CURDIR)/src/comp $(CC_OPT)' dgd
	cd src/lpc; MAKEOBJDIR=$(PKG)/lpc $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CC_OPT)' 'CFLAGS= -I$(.CURDIR)/src/lpc -I$(.CURDIR)/src -I$(.CURDIR)/src/comp $(CC_OPT)' OBJ= package 
	cd src/host; MAKEOBJDIR=$(PKG)/host $(DMAKE) 'DMAKE=$(DMAKE)' 'CC=$(CC)' 'CCFLAGS=$(CC_OPT)' 'CFLAGS= -I$(.CURDIR)/src/host -I$(.CURDIR)/src $(CC_OPT)' dgd
	cd $(PK); $(LD) $(LD_OPT) -o driver $(OBJ) `cat comp/dgd lex/dgd ed/dgd kfun/dgd lpc/dgd host/dgd` $(LIBS)
	pkg_create -c $(PK)/+ONELINE -d $(PK)/+DESC -f $(PK)/+PLIST -r $(PK)/+REQ dgd_bin

install:
	@cd src; $(DMAKE) install
	@tar cf - bin rundgd lpmud doc/editor doc/kfun | (cd $(BASEDIR); tar xf -)
	@cp -p src/Copyright $(BASEDIR)
	@ [ ! -d $(BASEDIR)/$(MUDLIB) ]; then \
		tar cf - $(MUDLIB) | (cd $(BASEDIR); tar xf -); \
	fi
	@cd $(BASEDIR); if [ ! -d tmp ]; then mkdir tmp; chmod 750 tmp; fi; \
	  chown -R mud.mudadmin .

clean:
	@cd $(PK); rm -rf driver *.o comp lex ed kfun lpc host
	@cd src; $(DMAKE) clean

.include <bsd.prog.mk>
