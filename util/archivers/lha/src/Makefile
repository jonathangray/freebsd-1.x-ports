PROG=lha
CFLAGS+= -DEUC -DGETTIMEOFDAY -DMKTIME -DTZSET -DSYSV_SYSTEM_DIR
SRCS=lharc.c lhadd.c lhlist.c lhext.c header.c append.c crcio.c dhuf.c \
	extract.c huf.c larc.c maketbl.c maketree.c shuf.c slide.c \
	util.c patmatch.c

NOMAN=noman
BINDIR=/usr/local/bin

.include <bsd.prog.mk>
