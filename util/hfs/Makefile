# $Id: Makefile,v 1.2 1994/05/15 06:19:07 rich Exp $

PROG=		hfs
SRCS=		main.cxx btree.cxx catalog.cxx fs.cxx volume.cxx misc.cxx \
		paranoid.cxx status.cxx file.cxx args.cxx extent.cxx ls.cxx \
		mac.cxx copy.cxx mdirent.cxx glob.c
LDFLAGS+=	-static
LDADD+=		-lg++

MAN1=		hfs.1
MLINKS+=	hfs.1 hls.1 hfs.1 hcd.1 hfs.1 hdir.1 hfs.1 hpwd.1 hfs.1 hcat.1 hfs.1 hread.1

ALIASES=	hls hcd hdir hpwd hcat hread
afterinstall:
	cd $(DESTDIR)$(BINDIR); \
	for name in $(ALIASES); do \
		 ln -s hfs $$name; \
	done

# 1 iff little endian (i.e. i386), 0 iff big endian

LITTLEENDIAN	= 1

# set to 1 if to check structure packing and "endian-ness"

PARANOID	= 1

# set to name of default device to use for disk access

DEFAULT_DISK_DEVICE	= "\"a:\""

# set to name of file stored in user's home directory holding
# volume name and current directory

STAT_FILE		= "\".macdir\""

# set to the directory separator character for the OS

DIR_SEP			= "'/'"

# include following defines for debugging options

DEBUG_COMMANDS		= \
#			  -DEXTENDED_ERRORS \
#			  -DDUMP_INDEX_NODES \
#			  -DDUMP_LEAF_NODES \

CFLAGS+=	-DLITTLEENDIAN=$(LITTLEENDIAN) \
		-DPARANOID=$(PARANOID) \
		-DDEFAULT_DISK_DEVICE=$(DEFAULT_DISK_DEVICE) \
		-DSTAT_FILE=$(STAT_FILE) \
		-DDIR_SEP=$(DIR_SEP) \
		$(DEBUG_COMMANDS)


package: pkg_bin

pkg_bin: install
	pkg_create -c pkg/+ONELINE -d pkg/+DESC -f pkg/+PLIST hfs_bin

.include <bsd.prog.mk>
