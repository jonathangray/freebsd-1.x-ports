/*
 * interface to the transmit batch files
 */

/* tunable parameters */
#ifndef FLUSHEVERY
#define FLUSHEVERY 1	/* fflush batch files every this many lines */
#endif			/* FLUSHEVERY */
#ifndef NOPENBFS
#define NOPENBFS 10	/* # (master) batchfiles kept open for batching */
			/* upper bound computable with dup */
			/* could be NBFSPERMBF given dynamic allocation */
#endif			/* NOPENBFS */

struct batchfile {
#ifdef notdef
	int bf_ref;			/* reference count */
#endif
	FILE *bf_str;			/* stream */
	char *bf_name;			/* file name */
	char *bf_msgid;			/* last message id written */
	short bf_lines;			/* until fflush */
};

/* imports from trbatch.c */
extern struct batchfile *bfopen(), *bfisopen();
extern statust bffkclose(), bfrealclose();
extern int bfflush();
/* imports from trbatcomm.c */
extern statust bfclose(), bfrclose();
extern struct batchfile *bfincache(), *fakebf();

extern struct batchfile batchfile[];	/* try to keep open always */
#define lastbf &batchfile[NOPENBFS-1]
/*
 * More than one pointer in ordtobfs may point at a given batchfile,
 * to permit sharing of open batch files among multiple sys entries.
 * ordtobfs[ordinal # of batch sys entry] -> (usually open) batch file,
 * if the index is in range.
 */
extern struct batchfile *ordtobfs[];
