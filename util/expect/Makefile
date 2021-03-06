# Generated automatically from Makefile.in by configure.
#
# Makefile for Expect
#

VERSION = \"5.7.0\"
SHORT_VERSION = 5.7

# Requires at least Tcl 7.0
# Known to work with up to Tcl 7.3

# While Tk is optional, if you do use Tk, it must be at least Tk 3.3
# Known to work with up to Tk 3.4

srcdir 	= .

######################################################################
# The following lines are things you may want to change
######################################################################

# Tcl include files.  (If you haven't installed Tcl yet, read the README file).
# This must point to the directory that contains ALL of Tcl's include
# files, not just the public ones.
TCLHDIR = -I./../tcl

# Tcl library.  Should be something like -ltcl or ../tcl/libtcl.a
TCLLIB = `if [ -f ../tcl/libtcl.a ]; then echo ../tcl/libtcl.a; else echo -ltcl; fi`

# The following three defs are only nec. if you want to use Tk with Expect.
# Tk include files
TKHDIR = $(srcdir)/../tk
# Tk library
TKLIB =  `if [ -f ../tk/libtk.a ]; then echo ../tk/libtk.a; else echo -ltk; fi`
# X11 include files
X11HDIR = -I/usr/X386/include

# flags to pass to both cc and ld
# -O for production version
# -g for debuggable version
CFLAGS = -g

# flags to pass only to linker (after .o files but before libraries)
# If you are on a NeXT, add "-m" flag to avoid complaints about multiple strtod
LFLAGS =

# which C compiler to use
CC = cc

# By default, `make install' will install the appropriate files in
# /usr/local/bin, /usr/local/lib, /usr/local/man, etc.  You can specify
# an installation prefix other than /usr/local here:
prefix = /usr/local

# You can specify a separate installation prefix for architecture-specific
# files such as binaries and libraries.
exec_prefix = /usr/local

# The following Expect scripts are not necessary to have installed as
# commands, but are very useful.  Edit out what you don't want installed.
# The INSTALL file describes these and others in more detail.
# Some Make's screw up if you delete all of them because SCRIPTS is a
# target.  If this is a problem, just comment out the SCRIPTS target itself.
SCRIPTS = timed-run timed-read ftp-rfc autopasswd lpunlock weather \
	passmass rftp kibitz rlogin-cwd xpstat tkpasswd dislocate xkibitz \
	tknewsbiff
# A couple of the scripts have man pages of their own.
# You can delete these too if you don't want'em.
SCRIPTS_MANPAGES = kibitz dislocate xkibitz tknewsbiff

# Short directory path where expect binary can be found to support #! hack.
# This directory path can be the same as the directory in which the binary
# actually sits except when the path is so long that the #! mechanism breaks
# (usually at 32 characters).
# The solution is to create a directory with a very short name, which consists
# only of symbolic links back to the true binaries.  Subtracting two for "#!"
# and a couple more for arguments (typically " -f" or " --") gives you 27
# characters.  Pathnames over this length won't be able to use the #! magic.
# For more info on this, see the execve(2) man page.
SHORT_BINDIR = $(exec_prefix)/bin

# If you have ranlib but it should be avoided, change this from "ranlib" #
# to something innocuous like "echo".  Known systems with this problem:
# older SCO boxes.
RANLIB = ranlib

# One of the following: select, poll, or simple
EVENT_TYPE = select
#
# You will need to change EVENT_TYPE to poll if your system is:
# NCR SVR4 (1.03.01)			where select is broken on ttys
# StarServer (SVR3 and SVR4.0)		where select is broken on ttys
# Pyramid OSx in the att universe	where select is not in libc.a
#
# You will need to change EVENT_TYPE to simple if your system is:
# old SCO				where select is broken on ptys
# 3b2 SVR3				where poll is broken on ptys
#    If you do use "simple":
# 1) you must also edit expect_cf.h and change
#    "#undef SIMPLE_EVENT" to "#define SIMPLE_EVENT",
# 2) you cannot use Expect with Tk since Tk's event manager insists on
#    reading all events through select, and
# 3) you cannot expect or interact with two or more processes simultaneously

######################################################################
# End of things you may want to change
#
# Do not change anything after this
######################################################################

bindir 	= $(exec_prefix)/bin
bindir_arch_indep = $(prefix)/bin
libdir 	= $(exec_prefix)/lib
tooldir = $(libdir)
datadir = $(prefix)/lib
execdatadir = $(exec_prefix)/lib

mandir 	= $(prefix)/man
man1dir = $(mandir)/man1
man3dir = $(mandir)/man3
infodir = $(prefix)/info
includedir = $(prefix)/include
oldincludedir =
docdir = $(datadir)/doc

# Where to store Expect's utility scripts - arch-independent and arch-non-
# independent.  (The first is currently just a placeholder since there are no
# arch-independent utility scripts.)  These correspond to the variables
# "exp_library" and "exp_exec_library".
SCRIPTDIR	= $(datadir)/expect
EXECSCRIPTDIR	= $(execdatadir)/expect

SHELL = /bin/sh

#INSTALL = `cd $(srcdir); pwd`/../install.sh -c
INSTALL = $(srcdir)/install.sh -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = ar
ARFLAGS = cr

# These let the DejaGnu test suite run when DejaGnu isn't 
# installed yet, so run it from the srcdir and objdir.
EXPECT = ` \
  if [ -f $${rootme}/expect ] ; then \
    echo $${rootme}/expect ; \
  else echo expect ; fi`

RUNTESTFLAGS =
RUNTEST = ` \
  if [ -f $${srcdir}/../dejagnu/runtest ] ; then \
    echo $${srcdir}/../dejagnu/runtest ; \
  else echo runtest ;  fi`

EVENT = exp_$(EVENT_TYPE)
PTY = pty_$(PTY_TYPE)
CFILES = exp_command.c expect.c $(PTY).c \
	exp_inter.c exp_regexp.c exp_tty.c \
	exp_log.c exp_main_sub.c exp_pty.c \
	exp_printify.c exp_trap.c exp_strf.c \
	exp_console.c exp_glob.c exp_win.c Dbg.c exp_clib.c
OFILES = exp_command.o expect.o $(PTY).o exp_inter.o exp_regexp.o exp_tty.o \
	exp_log.o exp_main_sub.o exp_pty.o exp_printify.o exp_trap.o \
	exp_console.o exp_strf.o exp_glob.o exp_win.o Dbg.o exp_clib.o
LIBEXPECT = libexpect.a
LIBEXPECTK = libexpectk.a

# X library
X11_LIB_FLAGS= -L/usr/X386/lib
X11_LIB = -lX11
X11_PROGS = expectk $(LIBEXPTK)

# expect must be setuid on crays in order to open ptys (and accordingly,
# you must run this Makefile as root).
# See the FAQ for more info on why this is necessary on Crays.
SETUID = @:
# SETUID = chmod u+s

# allow us to handle null list gracefully, "end_of_list" should not exist
SCRIPT_LIST = $(SCRIPTS) end_of_list
SCRIPT_MANPAGE_LIST = $(SCRIPTS_MANPAGES) end_of_list

# Define default parameters for ptys.  This is used when 1) running in the
# background, 2) user has not defined the variable STTY_INIT to initialize
# ptys, and 3) the pty-driver's defaults suck.
# If your system doesn't understand "sane", try "cooked".  Users with 8-bit
# character sets may need to disable parity.
STTY=\"sane\"

PTY_TYPE = termios

CPPFLAGS =	-I. -I$(srcdir) $(TCLHDIR) $(TKHDIR) $(X11HDIR) \
		-DVERSION=$(VERSION) \
		-DDFLT_STTY=$(STTY) \
		-DSCRIPTDIR=\"$(SCRIPTDIR)\" \
		-DEXECSCRIPTDIR=\"$(EXECSCRIPTDIR)\" \
		-DTCL_DEBUGGER

#
# Flags to pass to cc (i.e. add to the end of the CFLAGS line below).
# Note that setting one will not set others automatically.  Set all that
# are relevant.
#
# NOTE THAT THESE FLAGS ARE NO LONGER SUPPORTED.  THE FUNCTIONALLY IS REPLACED
# BY THE AUTOMATIC CONFIGURATION CODE.  ONLY MESS WITH THE FOLLOWING DEFS IF
# YOU ARE POSITIVE THE AUTO CONFIG CODE IS FAILING.
# 
# -DSYSV3 if you are running SVR3 or later.
# -DSYSV4 if you are running SVR4.  This option does not preclude -DSYSV3.
# -DAUX2 if you are running Mac A/UX 2.
# -DMIPS_BSD if you are on a Mips machine using the BSD universe.
# -D_BSD_SIGNALS if you are on a Silicon Graphics AND want BSD semantics when
#	using the expect library.  Otherwise, you are better off just sticking
#	with rearming signals.

# Flags to pass to ld (i.e., add to the end of the LIBS line below).
#
# -lc -lBSD     If you are using the BSD compatibility library on an HP/UX,
#		force libc.a to be loaded first.
# -lsocket	For SCO UNIX 3.2.2
#  /usr/ucblib/libucb.a is needed for solaris 2.0 after -lm
LIBS = $(LFLAGS) -lm

CFLAGS_INT = $(MH_CFLAGS) $(CPPFLAGS) $(CFLAGS)

.c.o:
	$(CC) -c $(CFLAGS_INT) $(HDEFS) $<

all: expect $(LIBEXPECT) ${X11_PROGS}

info:
dvi:

expect:	$(OFILES) exp_main_exp.o $(EVENT).o
	$(CC) $(CFLAGS) -o expect $(OFILES) exp_main_exp.o $(EVENT).o $(TCLLIB) $(LIBS) 
	$(SETUID) expect

# Build Expect with TestCenter
expect.tc:	$(OFILES) exp_main_exp.o $(EVENT).o
	proof $(CC) $(CFLAGS) -o expect.tc $(OFILES) exp_main_exp.o $(EVENT).o $(TCLLIB) $(LIBS) 
	$(SETUID) expect

expectk:        $(OFILES) exp_main_tk.o exp_tk.o
	$(CC) $(CFLAGS) -o expectk $(OFILES) exp_main_tk.o exp_tk.o $(TKLIB) $(TCLLIB) $(X11_LIB_FLAGS) $(X11_LIB) $(LIBS) 
	$(SETUID) expectk

test:
	rm -f .tmp
	echo "set objdir" `pwd` > .tmp
	if [ "$(srcdir)" = "." ] ; then \
	   echo "set srcdir" `pwd` >> .tmp ; \
	else echo "set srcdir" $(srcdir) >> .tmp ; fi
	echo "cd \$${srcdir}/tests" >> .tmp
	echo "source all" >> .tmp
	rootme=`pwd`; export rootme; \
	srcdir=${srcdir} ; export srcdir ; \
	if [ -f ./expect ] ; then  \
	   TCL_LIBRARY=$${srcdir}/../tcl/library ; \
	   export TCL_LIBRARY ; fi ; \
	./expect -f .tmp
	rm -f .tmp

###	@( echo set objdir `pwd`\; cd ./tests\; source all ) | ./expect -

Makefile : $(srcdir)/Makefile.in
	$(SHELL) config.status

$(LIBEXPECT): $(OFILES)
	$(AR) $(ARFLAGS) $(LIBEXPECT) $(OFILES)
	-$(RANLIB) $(LIBEXPECT)

$(LIBEXPECTK): $(OFILES) exp_tk.o
	$(AR) $(ARFLAGS) $(LIBEXPECTK) $(OFILES) exp_tk.o
	-$(RANLIB) $(LIBEXPECTK)

.PHONY: install-info install info
install-info:
# CYGNUS LOCAL: don't install scripts
install:	expect ${X11_PROGS} $(LIBEXPECT) # $(SCRIPTS)
# install Expect
	$(INSTALL_PROGRAM) expect $(bindir)/expect
# install Expectk if present
	-if [ -s expectk ] ; then \
		$(INSTALL_PROGRAM) expectk $(bindir)/expectk ; \
	fi
# install Expect man page
	$(INSTALL_DATA) $(srcdir)/expect.man $(man1dir)/expect.1
# install Expect library
	$(INSTALL_DATA) $(LIBEXPECT) $(libdir)/$(LIBEXPECT)
	-$(RANLIB) $(libdir)/$(LIBEXPECT)
# install man page for Expect and Expectk libraries
	$(INSTALL_DATA) $(srcdir)/libexpect.man $(man3dir)/libexpect.3
# install Expect-Tk library if present
	-if [ -s $(LIBEXPECTK) ] ; then \
		$(INSTALL_DATA) $(LIBEXPECTK) $(libdir)/$(LIBEXPECTK).n ; \
		$(RANLIB) $(libdir)/$(LIBEXPECTK).n 2>>/dev/null ; \
	fi
# install Expect's public include files
	$(INSTALL_DATA) expect_cf.h $(includedir)
	$(INSTALL_DATA) $(srcdir)/expect.h $(includedir)
	$(INSTALL_DATA) $(srcdir)/expect_tcl.h $(includedir)
	$(INSTALL_DATA) $(srcdir)/expect_comm.h $(includedir)
# install Debugger's public include files (just in case it's not there)
	$(INSTALL_DATA) $(srcdir)/Dbg.h $(includedir)
# create utility-script directories
	@touch .x
	$(INSTALL_DATA) .x $(SCRIPTDIR)/.x
	$(INSTALL_DATA) .x $(EXECSCRIPTDIR)/.x
	@rm -f $(SCRIPTDIR)/.x $(EXECSCRIPTDIR)/.x
	@rm -f .x
	if ./expect $(srcdir)/fixcat ; then \
		$(INSTALL_DATA) $(srcdir)/fixcat $(EXECSCRIPTDIR)/cat-buffers ; \
	fi
# install standalone scripts and their man pages, if requested
#	-for i in $(SCRIPT_LIST) ; do \
#		if [ -f $$i ] ; then \
#			$(INSTALL_PROGRAM) $$i $(bindir_arch_indep)/$$i ; \
#			chmod a+rx $(bindir_arch_indep)/$$i ; \
#			rm $$i ; \
#		fi ; \
#	done
#	-for i in $(SCRIPT_MANPAGE_LIST) ; do \
#		if [ -f $(srcdir)/example/$$i.man ] ; then \
#			$(INSTALL_DATA) $(srcdir)/example/$$i.man $(man1dir)/$$i.1 ; \
#			chmod a+rx $(man1dir)/$$i.1 ; \
# 		fi ; \
#	done

#$(SCRIPT_LIST):
#	./expect $(srcdir)/fixline1 $(SHORT_BINDIR) < $(srcdir)/example/$@ > $@

clean:
	-rm -f *~ *.o core expect expectk dumb exho devtty $(LIBEXPECT) $(LIBEXPECTK)

check:	
	rootme=`pwd`; export rootme; \
	srcdir=${srcdir} ; export srcdir ; \
	EXPECT=${EXPECT} ; export EXPECT ; \
	if [ -f $${rootme}/expect ] ; then  \
	   TCL_LIBRARY=$${srcdir}/../tcl/library ; \
	   export TCL_LIBRARY ; fi ; \
	$(RUNTEST) $(RUNTESTFLAGS) --tool expect EXPECT=$$rootme/expect \
		--srcdir $(srcdir)/testsuite

distclean: clean
	-rm -f Makefile config.status expect_cf.h

configure: configure.in
	autoconf configure.in > configure
	-@chmod a+x configure

LINTFLAGS = -h -q -x 

lint:
	lint $(LINTFLAGS) $(CPPFLAGS) $(CFILES) exp_main_exp.c $(EVENT).c $(TCLLINTLIB) | tee expect.lint

##################################
# Following defines are conveniences for testing at NIST
##################################

nist:
	configure --v --prefix=/depot/tcl --exec-prefix=/depot/tcl/arch

epg:
	configure --v --prefix=/users/libes --exec-prefix=/users/libes/arch

mink:
	configure --v --prefix=/usr/tmp --exec-prefix=/usr/tmp/arch

cam:
	configure --v --prefix=/tmp_mnt/home/fs1a/libes \
		      --exec-prefix=/tmp_mnt/home/fs1a/libes/arch

# report globals that shouldn't be public but are
bad_globals:
	nm $(LIBEXPECT) | egrep -v " [a-zU] | _exp| _Exp| _Dbg"
	nm $(LIBEXPECTK) | egrep -v " [a-zU] | _exp| _Exp| _Dbg"	

#########################################
# Following defs are for building with CodeCenter
#########################################

GCCROOT = /depot/gnu/arch/lib/gcc-lib/sparc-sun-sunos4.1/2.3.3
GCCLIB = $(GCCROOT)/libgcc.a
GCCINC = -I$(GCCROOT)/include
# following only on Sparcs
SABERDEFINE = -D__sparc__

# Following target builds expect under CodeCenter.
exp:	$(CFILES) exp_main_exp.c $(EVENT).c
	#load $(CPPFLAGS) $(CFILES) exp_main_exp.c $(EVENT).c $(TCLLIB) $(GCCLIB) $(LIBS)

# Following target builds expectk under CodeCenter.  Notes:
# Because of explicit #includes of <X11/...> in tk.h, you need to create
# a symlink from your X11 include directory to this directory
tk:     $(CFILES) exp_main_tk.c exp_tk.c
	#load $(CPPFLAGS) $(CFILES) exp_main_tk.c exp_tk.c $(TKLIB) $(TCLLIB) $(X11_LIB) $(LIBS)

# Follow definitions are for building expect and expectk under ObjectCenter
oexp:   $(CFILES) exp_main_exp.c $(EVENT).c
	#load $(CPPFLAGS) -C $(CFILES) exp_main_exp.c $(EVENT).c $(TCLLIB)

otk:    $(CFILES) exp_main_tk.c exp_tk.c
	#load $(CPPFLAGS) -C $(CFILES) exp_main_tk.c exp_tk.c $(TKLIB)

FTPDIR = /proj/elib/online/pub/expect

# make a private tar file for myself
tar:	expect-$(SHORT_VERSION).tar
	mv expect-$(SHORT_VERSION).tar expect.tar

# make a public tar file and install it on ftp server
ftp:	expect-$(SHORT_VERSION).tar.Z expect-$(SHORT_VERSION).tar.gz
	cp expect-$(SHORT_VERSION).tar.Z $(FTPDIR)/alpha.tar.Z
	cp expect-$(SHORT_VERSION).tar.gz $(FTPDIR)/alpha.tar.gz
	cp HISTORY $(FTPDIR)
	rm expect-$(SHORT_VERSION).tar*
	ls -l $(FTPDIR)/alpha.tar*

expect-$(SHORT_VERSION).tar:
	rm -f ../expect-$(SHORT_VERSION)
	ln -s `pwd` ../expect-$(SHORT_VERSION)
	rm -f ../pubfile
	ln pubfile ..
	cd ..;tar cvfh $@ `pubfile expect-$(SHORT_VERSION)`
	mv ../$@ .

expect-$(SHORT_VERSION).tar.Z:	expect-$(SHORT_VERSION).tar
	compress -fc expect-$(SHORT_VERSION).tar > $@

expect-$(SHORT_VERSION).tar.gz:	expect-$(SHORT_VERSION).tar
	gzip -fc expect-$(SHORT_VERSION).tar > $@

## dependencies will be put after this line... ##
Dbg.o: $(srcdir)/Dbg.c Dbg.h 
exp_command.o: $(srcdir)/exp_command.c expect_cf.h exp_tty.h \
	exp_rename.h expect.h exp_command.h \
	exp_log.h exp_printify.h exp_event.h exp_pty.h 
exp_inter.o: $(srcdir)/exp_inter.c expect_cf.h \
	exp_tty_in.h exp_tty.h exp_rename.h expect.h exp_command.h \
	exp_log.h exp_printify.h exp_regexp.h exp_tstamp.h
exp_log.o: $(srcdir)/exp_log.c expect_cf.h expect.h \
	exp_rename.h exp_log.h exp_printify.h 
exp_main_exp.o: $(srcdir)/exp_main_exp.c expect_cf.h \
	expect.h exp_rename.h exp_command.h exp_log.h exp_printify.h
exp_main_sub.o: $(srcdir)/exp_main_sub.c expect_cf.h \
	exp_rename.h \
	expect.h exp_command.h exp_tty_in.h exp_tty.h exp_log.h \
	exp_printify.h exp_event.h 
exp_main_tk.o: $(srcdir)/exp_main_tk.c expect_cf.h Dbg.h 
exp_poll.o: $(srcdir)/exp_poll.c expect_cf.h expect.h \
	exp_command.h exp_event.h 
exp_printify.o: $(srcdir)/exp_printify.c expect_cf.h 
exp_pty.o: $(srcdir)/exp_pty.c expect_cf.h exp_rename.h exp_pty.h 
exp_regexp.o: $(srcdir)/exp_regexp.c expect_cf.h \
	expect.h exp_regexp.h 
exp_select.o: $(srcdir)/exp_select.c expect_cf.h \
	expect.h exp_command.h exp_event.h 
exp_simple.o: $(srcdir)/exp_simple.c expect_cf.h \
	expect.h exp_command.h exp_event.h 
exp_strf.o: $(srcdir)/exp_strf.c 
exp_tk.o: $(srcdir)/exp_tk.c expect_cf.h expect.h exp_command.h exp_event.h 
exp_trap.o: $(srcdir)/exp_trap.c expect_cf.h expect.h \
	exp_command.h exp_log.h exp_printify.h 
exp_tty.o: $(srcdir)/exp_tty.c expect_cf.h \
	expect.h exp_rename.h exp_tty_in.h exp_tty.h exp_log.h \
	exp_printify.h exp_command.h 
exp_win.o: $(srcdir)/exp_win.c exp_win.h 
expect.o: $(srcdir)/expect.c expect_cf.h \
	exp_rename.h expect.h exp_command.h \
	exp_log.h exp_printify.h exp_event.h exp_tty.h exp_tstamp.h
lib_exp.o: $(srcdir)/lib_exp.c expect_cf.h exp_rename.h expect.h \
	exp_printify.h
pty_sgttyb.o: $(srcdir)/pty_sgttyb.c expect_cf.h exp_rename.h exp_tty_in.h \
	exp_tty.h exp_pty.h 
pty_termios.o: $(srcdir)/pty_termios.c expect_cf.h exp_win.h \
	exp_tty_in.h exp_tty.h exp_rename.h exp_pty.h 
pty_unicos.o: $(srcdir)/pty_unicos.c expect_cf.h exp_rename.h 
