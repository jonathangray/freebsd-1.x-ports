##makefile for lynx
##lynx written by Lou Montulli University of Kansas

##change the next line if you want lynx installed somewhere 
##besides /usr/local/bin

exec= /usr/local/bin

##change the next line if you want the lynx man file installed somewhere
##besides /usr/man/man1

doc= /usr/local/man/man1

##change the next line if you want the lynx.cfg file installed somewhere
##besides /usr/local/lib

cfg= /usr/local/lib

installbin= install -c -s -m 555
installdoc= install -c -m 444

#set the relative location of the WWW library Implementation directory, 
#from this directory
#do not set this to be an absolute path!!!
WWWINC= WWW/Library/Implementation

# !!!!!!!!!!!!!  DIRECT WAIS ACCESS !!!!!!!!!!!!!!!!!!!!!!!!!!
# If you are adding direct wais access you must fill in the
# location of wais.a, client.a and the standard math library, libm.a.  
# You can pick up the freeWAIS source from sunsite.unc.edu  
# or from ftp2.cc.ukans.edu.
# Plese put an ABSOLUTE address here if you want it to work! 
#WAISLIB= ../../freeWAIS-0.202/bin/client.a ../../freeWAIS-0.202/bin/wais.a /usr/lib/libm.a


#
# if you are on one of the supported systems (see all: list below)
# you should not have to modify anything from here down!
# If you are not, then you should edit the generic: entry below.
#
all: freebsd

run:
	@echo
	@echo "Please run make with one of the following arguments"
	@echo "aix32      -- for RS/6000 running AIX 3.2"
	@echo "aix31      -- for RS/6000 running AIX 3.1 with curses bug"
	@echo "apollo     -- apollo systems (untested)"
	@echo "bsdi       -- BSD Interactive (untested)"
	@echo "freebsd    -- for FreeBSD"
	@echo "generic    -- for generic UNIX"
	@echo "linux      -- for PC linux (untested)"
	@echo "next       -- for NeXT OS"
	@echo "osf        -- for Alpha OSF/1"
	@echo "sco        -- for SCO machines (untested)"
	@echo "sgi        -- for SGI machines (untested)"
	@echo "sun3       -- for SUN 3 OS"
	@echo "sun4       -- for SUN 4 OS"
	@echo "svr4       -- for SVR4"
	@echo "snake      -- for HP-UX (untested)"
	@echo "solaris2   -- for Solaris 2.x (untested)"
	@echo "ultrix     -- for DEC Ultrix"
	@echo "decstation -- for DEC Ultrix (same as ultrix above)"
	@echo "low_curses -- for generic systems with faulty curses"
	@echo "clean      -- removes all '.o' and 'core' files"
	@echo "tar        -- runs clean, removes executable, and tars the whole directory"
	@echo "compress   -- runs tar, then compresses the result"
	@echo

# Map some extra commands to existing ones
unix: generic
decstation: ultrix

# if you are compiling on a previously unsupported system, modify
# this generic entry!!
#
# defines include:
# -DNO_CUSERID     if you don't have cuserid()
# -DNO_UTMP	   if you don't have utmp capibilities
# -Dvfork=fork     if you don't have vfork()
# -DMMDF	   if you use MMDF instead of sendmail
# -DFANCY_CURSES   enables fancy curses (bold, underline, reverse)
# -DNO_KEYPAD      if you don't have keypad() and related defines
# -DNO_SIZECHANGE  Disables window sizechange code
# -DNO_CPU_LIMIT   Disables Max CPU timeout (includes CLOCKS_PER_SECOND)
#
#generic machines with original Berkeley curses
generic:  
	@echo "You must first compile the WWW library in WWW/Library"
	cd WWW/Library/unix; make
	cd src; make all CC="cc" MCFLAGS="-g -DFANCY_CURSES -DUNIX \
		-I../$(WWWINC)" LIBS="-lcurses -ltermcap $(WAISLIB)" \
		WWWLIB="../WWW/Library/unix/libwww.a"

# Tested on DN 3500 & 4500's running Domain O/S 10.4 BSD
# -Acpu,mathchip may have to be changed on other machines - see man cpuhep
apollo:
	cd WWW/Library/apollo_m68k; make
	cd src; make all CC="cc" MCFLAGS="-g -DUNIX -DNO_KEYPAD -DNO_CUSERID\
		-D_BUILTINS -W0,-opt,4 -I../$(WWWINC)" \
		LIBS="-lcurses -ltermcap $(WAISLIB)" \
		WWWLIB="../WWW/Library/apollo_m68k/libwww.a $(MYWWWLIB)"

snake:  
	cd WWW/Library/snake; make
	cd src; make all CC="gcc" MCFLAGS="-g -DFANCY_CURSES -DUNIX \
		-DSNAKE -I../$(WWWINC)" LIBS="-lcurses -ltermcap \
		$(WAISLIB)" WWWLIB="../WWW/Library/snake/libwww.a "

svr4:  
	cd WWW/Library/svr4; make
	cd src; make all CC="cc" MCFLAGS="-g -DFANCY_CURSES -DUNIX \
		-I../$(WWWINC) -DNO_UTMP -DSVR4" \
		LIBS="-lcurses -lsocket -lnsl $(WAISLIB)" \
		WWWLIB="../WWW/Library/svr4/libwww.a"

# SCO doesn't like line continuers?  (although others have said it does :)
sco:  
	cd WWW/Library/unix; make CC="cc"
	cd src; make all CC="cc" MCFLAGS="-g -DFANCY_CURSES -Dvfork=fork -DUNIX -DMMDF -I../$(WWWINC) -DNO_UTMP -DSCO -DNO_SIZECHANGE"  LIBS="-lcurses -lsocket -ltermcap $(WAISLIB)"  WWWLIB="../WWW/Library/unix/libwww.a"

bsdi:  
	cd WWW/Library/Implementation; make -f BSDI_Makefile
	cd src; make all CC="cc" MCFLAGS="-g -DNO_CUSERID -DUNIX \
		-I../$(WWWINC) -DNO_UTMP -DSVR4 -DNO_KEYPAD" \
		LIBS="-lcurses -ltermcap $(WAISLIB)" \
		WWWLIB="../WWW/Library/svr4/libwww.a"

freebsd:
	cd WWW/Library/freebsd; make
	cd src; make all CC="cc" MCFLAGS="-O -DUNIX -DNO_KEYPAD \
		-I../$(WWWINC)" LIBS="-lcurses -ltermcap $(WAISLIB)" \
		WWWLIB="../WWW/Library/freebsd/libwww.a"

solaris2:  
	cd WWW/Library/solaris2; make
	cd src; make all CC="gcc" MCFLAGS="-g -DFANCY_CURSES -DUNIX \
		 -I../$(WWWINC) -DCURS_PERFORMANCE \
		-DNO_UTMP -DUSE_DIRENT -DSVR4" LIBS="-ltermlib $(WAISLIB)\
 		-lnsl -lsocket" WWWLIB="../WWW/Library/solaris2/libwww.a"


#osf doesn't much care for '\' as a line continuer?
osf:            
	cd WWW/Library/osf; make
	cd src; make all CC="cc" MCFLAGS="-g -DFANCY_CURSES -DUNIX $(ADDFLAGS)\
		-I../$(WWWINC)" LIBS="-lcurses -ltermcap \
		$(WAISLIB)" WWWLIB="../WWW/Library/osf/libwww.a"

dbg:   
	cd WWW/Library/osf; make CC="gcc"
	cd src; make all CC="gcc" MCFLAGS="-g -Wall $(ADDFLAGS) -DFANCY_CURSES \
		-DUNIX -I../$(WWWINC)" LIBS="-lcurses -ltermcap \
		$(WAISLIB)" WWWLIB="../WWW/Library/osf/libwww.a"


#sgi doesn't like '\' as a line continuer?
sgi:
	cd WWW/Library/sgi; make
	cd src; make all CC="cc" MCFLAGS="-g -cckr -DFANCY_CURSES  -DUNIX -Dvfork=fork -I../$(WWWINC) -DNO_UTMP" LIBS="-lcurses -ltermcap -DSYSV -DPERFORMANCE $(WAISLIB)" WWWLIB="../WWW/Library/sgi/libwww.a"

linux:
	cd WWW/Library/unix; make cc="gcc"
	cd src; make all CC="gcc" MCFLAGS="-g -DUNIX -DNO_KEYPAD -DNO_TTYTYPE -I../$(WWWINC)" LIBS="-lcurses -ltermcap $(WAISLIB)" WWWLIB="../WWW/Library/unix/libwww.a"

aix32: 
	cd WWW/Library/rs6000; make
	cd src; make all CC="cc" MCFLAGS="-g -DFANCY_CURSES -DUNIX \
		-I../$(WWWINC)" LIBS="-lcurses -ltermcap -lbsd \
  		$(WAISLIB)" WWWLIB="../WWW/Library/rs6000/libwww.a"

aix31: 
	cd WWW/Library/rs6000; make
	cd src; make all CC="cc" MCFLAGS="-g -DUNIX \
		-I../$(WWWINC)" LIBS="-lcurses -ltermcap -lbsd \
		$(WAISLIB)" WWWLIB="../WWW/Library/rs6000/libwww.a"

ultrix:  
	cd WWW/Library/decstation; make
	cd src; make all CC="cc" MCFLAGS="-g -Olimit 600 -DFANCY_CURSES \
		 -DUNIX -DULTRIX -I../$(WWWINC)" \
		LIBS="-lcursesX -ltermcap $(WAISLIB)" \
		WWWLIB="../WWW/Library/decstation/libwww.a "

sun:
	@echo "Run make again and use \"make sun3\" or \"make sun4\""

#
# add -lresolv to the last "LIBS" entry within the quotes if needed
#
sun3:    
	cd WWW/Library/sun3; make
	cd src; make all CC="gcc" MCFLAGS="-g -DUNIX -DFANCY_CURSES \
		 -I../$(WWWINC) -DSUN -I/usr/5include"\
		LIBS="-lcurses -ltermcap -L/usr/5lib -lresolv $(WAISLIB)" \
		WWWLIB="../WWW/Library/sun3/libwww.a"

#
# add -lresolv to the last "LIBS" entry within the quotes if needed
#
sun4:
	cd WWW/Library/sun4; make
	cd src; make all CC="gcc" MCFLAGS="-g -DUNIX -DFANCY_CURSES \
		-I../$(WWWINC) -DSUN -I/usr/5include" \
		LIBS="-lcurses -ltermcap -L/usr/5lib -lresolv $(WAISLIB)" \
		WWWLIB="../WWW/Library/sun4/libwww.a"

next: 
	cd WWW/Library/next; make
	cd src; make all CC="cc" MCFLAGS="-g -DUNIX -DNEXT -DNO_CUSERID\
		-DNO_KEYPAD -I../$(WWWINC) -I../$(WWWINC)" LIBS="-lcurses \
		-ltermcap $(WAISLIB)" WWWLIB="../WWW/Library/next/libwww.a"

traversal:
	cd WWW/Library/rs6000; make
	cd src; make traversal CC="cc" MCFLAGS="-g -DFANCY_CURSES -DUNIX  \
		-DTRAVERSAL -I../$(WWWINC)" LIBS="-lcurses -ltermcap \
		-lbsd $(WAISLIB)" WWWLIB="../WWW/Library/rs6000/libwww.a"

clean:
	rm -f WWW/Library/*/*.[aob]
	cd src; make clean
	rm -f *.b lynx core

tar:  clean
	rm -f src/lynx
	rm -f lynx
	rm -f src/a.out
	make save

zipcompress: compress zip
	echo "done!"

zip:  clean
	rm -f src/lynx
	rm -f lynx
	rm -f src/a.out
	rm -f ../lynx2-2.zip
	cd ..; zip -r lynx2-2.zip lynx2-2

save:
	cd ..; tar -cf - lynx2-2 > lynx2-2.tar

compress: tar
	cd ..; compress -f -v lynx2-2.tar

install: all
	-mv -f $(exec)/lynx $(exec)/lynx.old
	-mv -f $(cfg)/lynx.cfg $(cfg)/lynx.oldcfg
	$(installbin) lynx $(exec)/lynx
	$(installdoc) lynx.man $(doc)/lynx.1
	$(installdoc) lynx.cfg $(cfg)/lynx.cfg

lynx_bin:
	pkg_create -f pkg/+CONTENTS -d pkg/+DESC -p /usr/local \
		-c -"LYNX, Version 2.2" lynx_bin.tgz

pkg:    lynx_bin
