# /usr/ports/Makefile V2.3	by Julian Stacey <stacey@guug.de>
#	Copyright Julian Stacey, Munich Dec. 93, Free Software, No Liability.
#	For details see `Legalities' in /sys/Makefile.

# This Makefile & ports/*/Makefile are still in development.
# Some comments detailing bugs are suposedly no longer valid,
# but until I can manage to sup all of ports/ I can't prove bugs are gone,
# so comments stay for a while as a warning to check.

MAKE_X_MIT ?= YES

world:	display tools .configured objj _SUBDIRUSE all
	@# _SUBDIRUSE with bsd.subdir_ports.mk does a make world in subdirs
	@# most ports/_whatever_/Makefile(s) & bsd.subdir_ports.mk dont know to
	@# do a make all after world, so its done here
	@# (if it was in bsd.subdir_ports.mk it'd get done twice)

objj:
	@# maybe later add some obj sym link creation here ?

tools:
	#	@#Making prerequisite tools.
	#	@echo "==={> $$i devel"
	#	cd devel ; make tools	# devel/gmake	--> /usr/gnu/bin/gmake
	#	@echo "<}=== $$i devel"

display:
	@echo	DESTDIR is	${DESTDIR}
	@echo	TOP is		${TOP}
	@echo	AVOID is	${AVOID}
	@echo	MAKE is		${MAKE}
	@echo	GMAKE is	${GMAKE}
	@echo	WAIT_OK is	${WAIT_OK}
	@echo	DESTDIR is	${DESTDIR}
	@echo	BINDIR is	${BINDIR}
	@echo	MAKE_X_MIT is	${MAKE_X_MIT}

#	cleandist:
#	.if !defined(NOCLEANDIR)
#		@echo " Cleaning up the source tree, and rebuilding the obj tree"
#		here=`pwd`; dest=/usr/obj/`echo $$here`; \
#		cd $$dest; rm -rf ${SUBDIR}
#		find . -name obj | xargs -n30 rm -rf
#	.if defined(MAKE_LOCAL) & exists(local) & exists(local/Makefile)
#		# The cd is done as local may well be a symbolic link
#		-cd local ; find . -name obj -type l | xargs -n30 rm -rf
#	.endif
#	.if defined(MAKE_PORTS) & exists(ports) & exists(ports/Makefile)
#		-cd ports ; find . -name obj -type l | xargs -n30 rm -rf
#	.endif
#		make cleandir
#		make obj
#	.endif

after_all:
.if !defined(AVOID)
	@echo "Making packages that require special make labels."
	-cd xview; ${MAKE} World # ar: notifydata.o: Too many open files
.endif

clean:	_SUBDIRUSE
	@# Need to do this find manually (how horrible):
	-find shell/bash print/tex x11/urt -name \*.o -exec rm {} \;
	-rm -f .configured

# pkg:
#	done in bsd.subdir_ports.mk

.configured:
.if defined(NEVER_CLUDGE)
	@# All this stuff will go, once a clean sup & make proves it
	@# can be discarded
	@echo "Started configuring."
	# Start of some temporary nasty cludges for broken packages
	@# for tcl-dp
	-ln -s /usr/src/../ports /usr/ports
	@echo Cludging for syscons_xtra
	cd /usr/src/sys/sys ; ln -s ../i386/include/console.h console.h
	@echo Cludging for rzsz
	-mkdir /usr/local/man/cat1
	-cd math/octave/octave; ./configure
	-cd ilocal/wu-ftpd; chmod +x build; ./build bsd
	-cd mail/elm; ./configure
	#	PENDING Rod says:
	#	If you don't do the configure you end up with elm thinking your
	#	domain is cdrom.com due to brain damaged code that compiles
	#	the domain name into the binary!
	-mkdir /usr/ingres /usr/ingres/lib /usr/ingres/bin
	@echo "Doing things that need a '${MAKE} World' first time (only)."
	# Things that need a ${MAKE} World ever time (such as xview)
	# are not done here.
	@echo "Seeing if user news exists (wanted by inn)."
	grep news /etc/passwd
	@echo "Finished configuring."
.endif
	-date > $@

# Directories each containing numerous packages
SUBDIR =
G_SUBDIR =

SUBDIR += audio
SUBDIR += comm
SUBDIR += db
SUBDIR += devel
SUBDIR += editor
SUBDIR += lang
SUBDIR += mail
SUBDIR += math
SUBDIR += net
SUBDIR += news
SUBDIR += print
SUBDIR += shell
SUBDIR += util
SUBDIR += x11

.if defined(MAKE_X_MIT) 
SUBDIR += x-mit	# do last as Very big
.endif

.include <bsd.subdir_ports.mk>

# End of Makefile.
