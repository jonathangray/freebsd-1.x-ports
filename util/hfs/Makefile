
#
# $Id: Makefile,v 1.1 1994/05/15 05:44:51 rich Exp $
# $Log: Makefile,v $
# Revision 1.1  1994/05/15 05:44:51  rich
# Initial revision
#
# Revision 1.11  1994/01/11  02:23:46  craigs
# Removed -g option
#
# Revision 1.10  1994/01/11  02:21:35  craigs
# Changed to version 0.3
#
# Revision 1.9  1994/01/06  03:40:41  craigs
# removed strip from dist target
#
# Revision 1.8  1994/01/06  03:40:03  craigs
# Removed dependencies from dist target
#
# Revision 1.7  1994/01/06  03:38:44  craigs
# Added COPYING
#
# Revision 1.6  1994/01/06  03:37:40  craigs
# Fixed dist target
#
# Revision 1.5  1994/01/06  03:17:58  craigs
# Final makefile
#
# Revision 1.4  1993/12/30  07:59:34  craigs
# Many changes - too numerous to mention!
#
# Revision 1.3  1993/12/16  20:12:01  craigs
# Lots of changes
#
# Revision 1.2  1993/11/25  22:00:40  craigs
# Added extra debug macros and updated dependencies
#
# Revision 1.1  1993/11/22  22:28:01  craigs
# Initial revision
#
# 

#
# start of user configurable items
#

#
# string used as basename for creating distribution files
DISTPREFIX	= hfs0_3

#
# set to 1 if processor is little endian (i.e. i386)
# set to 0 if processor is big endian (i.e. Sparc, 680x0, MIPS)
#
LITTLEENDIAN	= 1

#
# set to 1 if to check structure packing and "endian-ness"
#
PARANOID	= 1

#
# set to name of default device to use for disk access when 
#
DEFAULT_DISK_DEVICE	= "\"a:\""

#
# set to name of file stored in user's home directory holding
# volume name and current directory
#
STAT_FILE		= "\".macdir\""

#
# set to the directory separator character for the OS
#
DIR_SEP			= "'/'"

#
# include following defines for debugging options
#
DEBUG_COMMANDS		= \
#			  -DEXTENDED_ERRORS \
#			  -DDUMP_INDEX_NODES \
#			  -DDUMP_LEAF_NODES \

#
# directory to install in
#
#INSTALLPATH		= /usr/local
INSTALLPATH		= $(HOME)
BININSTALLDIR		= $(INSTALLPATH)/bin
MANINSTALLDIR		= $(INSTALLPATH)/man/man.1

#
# end of user configurable items
#

PROG		= hfs
MAN		= hfs.1
DOC		= hfs.doc
ALIASES		= hls hcd hdir hpwd hcat hread
CXXSOURCES	= btree.cxx catalog.cxx fs.cxx main.cxx volume.cxx misc.cxx \
		  paranoid.cxx status.cxx file.cxx args.cxx extent.cxx ls.cxx \
		  mac.cxx copy.cxx mdirent.cxx 
CSOURCES	= glob.c
SOURCES		= $(CXXSOURCES) $(CSOURCES)
HEADERS		= mac.h btree.h catalog.h fs.h volume.h mdir.h \
		  misc.h paranoid.h status.h file.h args.h endian.h \
		  error.h config.h extent.h ls.h cmd.h glob.h mdirent.h copy.h

EXTRAS		= Makefile README $(MAN) $(DOC) COPYING

DISTFILES	= $(SOURCES) $(HEADERS) $(EXTRAS)

#INCLUDEDIRS	= -Idir
#LIBDIRS	= -Ldir
#LIBS		= -ldir

DEFINES		= -DLITTLEENDIAN=$(LITTLEENDIAN) \
		  -DPARANOID=$(PARANOID) \
		  -DDEFAULT_DISK_DEVICE=$(DEFAULT_DISK_DEVICE) \
		  -DSTAT_FILE=$(STAT_FILE) \
		  -DDIR_SEP=$(DIR_SEP) \
		  $(DEBUG_COMMANDS)

#DEBUG		= -g
DEBUG		= -O6

#
# end of configurable items
#

OBJECTS		= $(CSOURCES:.c=.o) $(CXXSOURCES:.cxx=.o)

CCPLUSPLUS	= g++ -c
LD		= g++
LIBRARIAN	= ar rcs
INSTALL		= install
CP		= cp
LN		= ln -s

CFLAGS		= $(DEBUG) $(INCLUDEDIRS) $(DEFINES) -Wall
LDFLAGS		= $(LIBDIRS)

.SUFFIXES:	.cxx

.cxx.o:
	$(CCPLUSPLUS) $(CFLAGS) $< -o $@

all:	$(PROG) $(HEADERS)

install:	$(LIBFN) $(HEADERS)
	$(CP) $(PROG) $(BININSTALLDIR) ; \
	for f in $(MAN) ; do \
	  $(CP) $$f $(MANINSTALLDIR)\$(MAN) ; \
	done

aliases:
	for f in $(ALIASES) ; do \
          $(RM) -f $(BININSTALLDIR)/$$f ; \
  	  $(LN) -sf $(BININSTALLDIR)/$(PROG) $(BININSTALLDIR)/$$f ; \
        done

copies:
	for f in $(ALIASES) ; do \
          $(RM) -f $(BININSTALLDIR)/$$f ; \
  	  $(CP) $(BININSTALLDIR)/$(PROG) $(BININSTALLDIR)/$$f ; \
        done

$(PROG):	$(OBJECTS)
	$(LD) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBS)

$(DOC):		$(MAN)
	nroff -man $(MAN) | col -b > $(DOC)

clean:
	rm -f $(PROG) $(OBJECTS)


disk.image:
	cp /dev/fd0H1440 disk.image

depend:	
	md -u -- $(CFLAGS) -- $(SOURCES)

backup: $(DISTFILES) $(PROG) 
	tar -cvf - $(DISTFILES) $(PROG) | gzip > ../hfs.tgz

dist: 
	( cd ..; tar -cvf - $(DISTPREFIX)/* | gzip > $(DISTPREFIX).tgz )

zbackup: $(SOURCES) $(HEADERS) $(MAN) $(MAKEFILE)
	zip ../hfs.zip $(SOURCES) $(HEADERS) $(MAN) $(EXTRAS)

doscopy: $(SOURCES) $(HEADERS)
	for f in $(SOURCES) $(HEADERS) ; do \
          diff $$f /bambi/d/hfs/$$f > /dev/null; \
          if [ $$? -ne 0 ] ; then \
            echo $$f changed ; \
            cp $$f /bambi/d/hfs/$$f ; \
          fi ; \
        done


# Do not delete this line - make depend depends upon it

./btree.o : btree.cxx btree.h misc.h endian.h mac.h file.h fs.h volume.h error.h \
  catalog.h extent.h 
./catalog.o : catalog.cxx catalog.h btree.h /usr/g++-include/ostream.h misc.h \
  endian.h mac.h file.h fs.h volume.h error.h extent.h 
./fs.o : fs.cxx catalog.h btree.h /usr/g++-include/ostream.h misc.h endian.h mac.h \
  file.h extent.h error.h fs.h volume.h 
./main.o : main.cxx args.h btree.h misc.h endian.h mac.h file.h error.h catalog.h \
  fs.h volume.h extent.h config.h paranoid.h status.h cmd.h ls.h copy.h 
./volume.o : volume.cxx volume.h mac.h endian.h error.h misc.h 
./misc.o : misc.cxx misc.h endian.h 
./paranoid.o : paranoid.cxx endian.h 
./status.o : status.cxx misc.h endian.h status.h mac.h config.h 
./file.o : file.cxx misc.h endian.h fs.h volume.h mac.h error.h catalog.h btree.h \
  /usr/g++-include/ostream.h file.h extent.h 
./args.o : args.cxx args.h 
./extent.o : extent.cxx catalog.h btree.h /usr/g++-include/ostream.h misc.h endian.h \
  mac.h file.h fs.h volume.h error.h extent.h 
./ls.o : ls.cxx cmd.h endian.h misc.h args.h status.h mac.h fs.h volume.h error.h \
  catalog.h btree.h file.h extent.h 
./mac.o : mac.cxx mac.h endian.h 
./copy.o : copy.cxx cmd.h endian.h misc.h args.h status.h mac.h fs.h volume.h \
  error.h catalog.h btree.h file.h extent.h 
