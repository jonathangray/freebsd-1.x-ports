# Hey Emacs, this Makefile is in -*- text -*- mode!
#
# Makefile for Bash.
# If your cpp doesn't like -P, just get rid of it (the -P, not cpp).
# If you wish to use Gcc, then just type "make CC=gcc".
# If you wish to use GNU's Make, then change `MAKE'.
# If you don't like the destination, then change `bindir'.
# The file that you most likely want to look at is cpp-Makefile.
#
# If you haven't read README, now might be a good time.

bindir = /usr/gnu/bin
# MAKE    = make
RM      = rm -f
SHELL   = /bin/sh
GAWK     = awk
# GAWK     = gawk

# Force CPPNAME to be the name of your C preprocesor if Bash can't
# find it.  For instance, `CPPNAME=/usr/libexec/cpp' on 4.4 BSD.
#CPPNAME =
#CPP     = `$(CPPMAGIC) $(GETCPPSYMS) $(CPPNAME)` -P

# If all else fails, try this line instead.
CPP     = $(CC) -traditional -E

CPP_MAKEFILE = cpp-Makefile
ANSI_MAKEFILE = ansi-Makefile

# CPPFLAGS = $(SYSTEM) $(CPP_DEFINES)
CPPFLAGS = $(CPP_DEFINES)
CPP_ARGS = -DCPP_CC="$(CC)"

SUPPORTDIR     = ./support/
MKSYSDEFS      = $(SUPPORTDIR)mksysdefs
CPPMAGIC       = $(SUPPORTDIR)cppmagic
CAT_S          = $(SUPPORTDIR)cat-s
GETCPPSYMS     = $(SUPPORTDIR)getcppsyms
GETCPPSYMS_SRC = $(SUPPORTDIR)getcppsyms.c

# Here is a command which compresses runs of multiple blank lines to a
# single blank line.  "cat -s" works for BSD systems, but not for USG
# systems.  You can use an awk script if you like.  If you have too
# much trouble with this, just forget it.  It is for making
# bash-Makefile pretty and readable; something that isn't strictly
# necessary.
# SQUASH_BLANKS = cat -s
#
SQUASH_BLANKS = $(GAWK) -f $(CAT_S)

all:	.notified bash-Makefile
	$(MAKE) $(MFLAGS) $(MAKEARGS) -f bash-Makefile

bash-Makefile: $(CPP_MAKEFILE) Makefile machines.h sysdefs.h config.h
	@-if [ -f ansi-Makefile ]; then \
	    echo "cp ansi-Makefile tmp-Makefile.c"; \
	    cp ansi-Makefile tmp-Makefile.c; else \
	    echo "cp $(CPP_MAKEFILE) tmp-Makefile.c"; \
	    cp $(CPP_MAKEFILE) tmp-Makefile.c; \
	  fi
	$(RM) $(GETCPPSYMS)
	$(CC) -o $(GETCPPSYMS) $(GETCPPSYMS_SRC)
	rm -f bash-Makefile
	@/bin/sh -c 'echo $(CPP) $(CPPFLAGS) $(CPP_ARGS) tmp-Makefile.c \| $(SQUASH_BLANKS) \> bash-Makefile'
	@/bin/sh -c '$(CPP) $(CPPFLAGS) $(CPP_ARGS) tmp-Makefile.c | $(SQUASH_BLANKS) >bash-Makefile'
	rm -f tmp-Makefile.c

sysdefs.h: $(MKSYSDEFS)
	$(MKSYSDEFS)

# This is also performed by support/mksysdefs, but there's no way to change
# it if cpp-Makefile is changed without changing anything else, since there
# are no dependencies.  This lets you run `make ansi-Makefile'.
ansi-Makefile: $(CPP_MAKEFILE)
	cat cpp-Makefile | grep -v '/\*\*/' > $@

# Subsequent lines contain targets that are correctly handled by an
# existing bash-Makefile.

DEFINES install newversion mailable distribution architecture: bash-Makefile
	$(MAKE) $(MFLAGS) $(MAKEARGS) bindir=$(bindir) -f bash-Makefile $@

bash.tar.Z tags documentation clone: bash-Makefile directory-frob
	$(MAKE) $(MFLAGS) $(MAKEARGS) bindir=$(bindir) -f bash-Makefile $@

clean: bash-Makefile directory-frob
	rm -f .notified
	$(MAKE) $(MFLAGS) $(MAKEARGS) bindir=$(bindir) -f bash-Makefile $@

package:    pkg_bin

pkg_bin: install pkg/+DESC pkg/+ONELINE pkg/+PLIST
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST bash_bin

directory-frob:

.NOEXPORT:

.notified:
	@echo ""
	@echo "   You are about to make this version of GNU Bash for"
	@echo "   this architecture for the first time.  If you haven't"
	@echo "   yet read the README file, you may want to do so.  If"
	@echo "   you wish to report a bug in Bash, or in the installation"
	@echo "   procedure, please mail it to bash-maintainers@ai.mit.edu,"
	@echo "   and include:"
	@echo ""
	@echo "	* the version number of Bash,"
	@echo "	* the machine and OS that it is running on,"
	@echo "	* a description of the bug,"
	@echo "	* a recipe for recreating the bug reliably,"
	@echo "	* a fix for the bug if you have one!"
	@echo ""
	@touch .notified
	
