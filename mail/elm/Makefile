#
# : Makefile.SH,v 1.7 1993/12/28 05:34:07 smace Exp $
#
#  Makefile for the entire ELM mail system
#
#         (C) Copyright 1986,1987, by Dave Taylor
#         (C) Copyright 1988-1992, USENET Community Trust
#
#  $Log: Makefile,v $
#  Revision 1.11  1993/12/28 17:36:18  smace
#  Added clean dependency for the pkg_src target
#
# Revision 1.10  1993/12/28  05:58:30  smace
# Moved + file to pkg directory and re-ran Configure
#
# Revision 1.7  1993/12/28  05:34:07  smace
# Reran configure to get new Makefile.SH to corretnly substitute variables
#
# Revision 1.3  1993/10/09  19:36:02  smace
# Update to elm 2.4 pl23 release version
#
# Revision 5.3  1993/04/21  01:53:10  syd
# take out parallelism, effects standard make
#
# Revision 5.2  1993/04/21  01:26:18  syd
# Support parallel makes in subdirectories with e.g. "make J=2".
# From: decwrl!uunet.UU.NET!fin!chip (Chip Salzenberg)
#
# Revision 5.1  1992/10/03  22:12:17  syd
# Initial checkin as of 2.4 Release at PL0
#
# 
#

#	Variables established by Configure
CAT		=	/bin/cat
GREP		=	/usr/bin/grep
ECHO		=	/bin/echo
MAKE		=	/usr/bin/make
PR		=	/usr/bin/pr
SHAR		=	/usr/bin/shar -s 60000
SLEEP		=	/bin/sleep
TAR		=	/bin/tar
TAPE		=	unknown-remote-tape-unit

#	Other variables
SHELL			=	/bin/sh
SOURCE_FILES		=	`$(GREP) -v 'doc/' MANIFEST`
DISTRIBUTED_FILES	=	`$(CAT) MANIFEST`

# Targets
#	Give default target first and alone
default_target:	all

#	Targets that are simply executed in each subordinate makefile as is
all install uninstall lint clean:
		cd lib; $(MAKE) $(MJ) -$(MAKEFLAGS) $@
		cd src; $(MAKE) $(MJ) -$(MAKEFLAGS) $@
		cd utils; $(MAKE) $(MJ) -$(MAKEFLAGS) $@
		cd filter; $(MAKE) $(MJ) -$(MAKEFLAGS) $@
		cd doc; $(MAKE) $(MJ) -$(MAKEFLAGS) $@

#	Targets that apply to all elm at once
listing:
		@$(ECHO) "Making source listing in file LISTING."
		@$(PR) -n -f $(SOURCE_FILES) > LISTING

#	This is the only target that gets installed even if not out-of-date
#	with respect the files from which it is installed.
rmt-install:	rmt-defined
		@($(ECHO) "Warning: This assumes you have already installed";\
		  $(ECHO) "ELM on the remote machine <$(REMOTE)>. If this is";\
		  $(ECHO) "not the case, interrupt this process quickly!";\
		  $(SLEEP) 10;)
		cd lib; $(MAKE) $(MJ) -$(MAKEFLAGS) REMOTE=$(REMOTE) $@
		cd src; $(MAKE) $(MJ) -$(MAKEFLAGS) REMOTE=$(REMOTE) $@
		cd filter; $(MAKE) $(MJ) -$(MAKEFLAGS) REMOTE=$(REMOTE) $@
		cd utils; $(MAKE) $(MJ) -$(MAKEFLAGS) REMOTE=$(REMOTE) $@
		cd doc; $(MAKE) $(MJ) -$(MAKEFLAGS) REMOTE=$(REMOTE) $@

rmt-defined:
	@(if [ "$(REMOTE)" = "" ];\
	  then\
	    $(ECHO) "You need to define 'REMOTE' as the remote file system";\
	    $(ECHO) "for this particular command. The easiest way to do this";\
	    $(ECHO) "to type:";\
	    $(ECHO) "        make REMOTE=<remote file system> rmt-install";\
	    exit 1;\
	fi);

shar:
		$(SHAR) $(DISTRIBUTED_FILES)

tar:
		$(TAR) cvf $(TAPE) $(DISTRIBUTED_FILES)

#	Targets that are really in subordinate make files
documentation:
	cd doc; $(MAKE) $(MJ) -$(MAKEFLAGS) $@

elm:
	cd lib; $(MAKE) $(MJ) -$(MAKEFLAGS) all
	cd src; $(MAKE) $(MJ) -$(MAKEFLAGS) all

#	The dummy dependency here prevents make from thinking the target is the
#	filter directory that sits in the current directory, rather than
#	an abstract target.
filter: _filter

_filter:
	cd filter; $(MAKE) $(MJ) -$(MAKEFLAGS) all

#	The dummy dependency here prevents make from thinking the target is the
#	utils directory that sits in the current directory, rather than
#	an abstract target.
utils: _utils

_utils:
	cd utils; $(MAKE) $(MJ) -$(MAKEFLAGS) all


pkg:	pkg_src

pkg_src: clean pkg/+DESC_S pkg/+ONELINE_S pkg/+PLIST_S
	pkg_create -c pkg/+ONELINE_S -d pkg/+DESC_S -f pkg/+PLIST_S elm_src

