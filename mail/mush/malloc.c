/*
 * This is a slightly modified version of the malloc.c distributed with
 * Larry Wall's perl 2.0 sources.  RCS and sccs information has been
 * retained, but modified so that it will not actually affect checkin
 * or checkout of this file if revision control is used for Mush.
 *
 * Other changes include:
 *	Removal of the ASSERT macro and other code related to the
 *	preprocessor definition "debug"
 *
 *	Replaced #include "perl.h" with #include "mush.h" (guess why)
 *
 *	Warning messages are now printed with the mush Debug macro,
 *	that is, they are normally suppressed
 *
 *	Added a calloc() function, using mush's bzero()
 *
 * Also, the mush xfree() and free_vec() functions have been moved here.
 */

#include "mush.h"

/*
 * Compile this portion only if configured for INTERNAL_MALLOC
 */
#ifdef INTERNAL_MALLOC
#ifdef SYSV
#include <memory.h>
#define bcopy(src,dst,len)	memcpy(dst,src,len)
#endif /* SYSV */
#define free xfree	/* rename free for mush purposes */

/* Begin modified perl malloc.c */

/* Header: malloc.c,v 2.0 88/06/05 00:09:16 root Exp
 *
 * Log:	malloc.c,v
 * Revision 2.0  88/06/05  00:09:16  root
 * Baseline version 2.0.
 * 
 */

#ifndef lint
static char sccsid[] = "malloc.c	4.3 (Berkeley) 9/16/83";
#endif /* !lint */

#define RCHECK
/*
 * malloc.c (Caltech) 2/21/82
 * Chris Kingsley, kingsley@cit-20.
 *
 * This is a very fast storage allocator.  It allocates blocks of a small 
 * number of different sizes, and keeps free lists of each size.  Blocks that
 * don't exactly fit are passed up to the next larger size.  In this 
 * implementation, the available sizes are 2^n-4 (or 2^n-12) bytes long.
 * This is designed for use in a program that uses vast quantities of memory,
 * but bombs when it runs out. 
 */

/* I don't much care whether these are defined in sys/types.h--LAW */

#undef u_char
#define u_char unsigned char
#undef u_int
#define u_int unsigned int
#undef u_short
#define u_short unsigned short

/*
 * The overhead on a block is at least 4 bytes.  When free, this space
 * contains a pointer to the next free block, and the bottom two bits must
 * be zero.  When in use, the first byte is set to MAGIC, and the second
 * byte is the size index.  The remaining bytes are for alignment.
 * If range checking is enabled and the size of the block fits
 * in two bytes, then the top two bytes hold the size of the requested block
 * plus the range checking words, and the header word MINUS ONE.
 */
union	overhead {
	union	overhead *ov_next;	/* when free */
	struct {
		u_char	ovu_magic;	/* magic number */
		u_char	ovu_index;	/* bucket # */
#ifdef RCHECK
		u_short	ovu_size;	/* actual block size */
		u_int	ovu_rmagic;	/* range magic number */
#endif /* RCHECK */
	} ovu;
#define	ov_magic	ovu.ovu_magic
#define	ov_index	ovu.ovu_index
#define	ov_size		ovu.ovu_size
#define	ov_rmagic	ovu.ovu_rmagic
};

#define	MAGIC		0xff		/* magic # on accounting info */
#define OLDMAGIC	0x7f		/* same after a free() */
#define RMAGIC		0x55555555	/* magic # on range info */
#ifdef RCHECK
#define	RSLOP		sizeof (u_int)
#else /* !RCHECK */
#define	RSLOP		0
#endif /* RCHECK */

/*
 * nextf[i] is the pointer to the next free block of size 2^(i+3).  The
 * smallest allocatable block is 8 bytes.  The overhead information
 * precedes the data area returned to the user.
 */
#define	NBUCKETS 30
static	union overhead *nextf[NBUCKETS];
extern	char *sbrk();

static morecore();
static findbucket();

#ifdef MSTATS
/*
 * nmalloc[i] is the difference between the number of mallocs and frees
 * for a given block size.
 */
static	u_int nmalloc[NBUCKETS];
#endif /* MSTATS */

char *
malloc(nbytes)
	register unsigned nbytes;
{
	register union overhead *p;
	register int bucket = 0;
	register unsigned shiftr;

	if (nbytes == 0)
	    return NULL;
	/*
	 * Convert amount of memory requested into
	 * closest block size stored in hash buckets
	 * which satisfies request.  Account for
	 * space used per block for accounting.
	 */
	nbytes += sizeof (union overhead) + RSLOP;
	nbytes = (nbytes + 3) &~ 3; 
	shiftr = (nbytes - 1) >> 2;
	/* apart from this loop, this is O(1) */
	while (shiftr >>= 1)
		bucket++;
	/*
	 * If nothing in hash bucket right now,
	 * request more memory from the system.
	 */
	if (nextf[bucket] == (union overhead *)0)    
		morecore(bucket);
	if ((p = (union overhead *)nextf[bucket]) == (union overhead *)0)
		return (NULL);
	/* remove from linked list */
	if (*((int*)p) > 0x10000000)
	    Debug("Corrupt malloc ptr 0x%x at 0x%x\n",*((int*)p),p);
	nextf[bucket] = nextf[bucket]->ov_next;
	p->ov_magic = MAGIC;
	p->ov_index= bucket;
#ifdef MSTATS
	nmalloc[bucket]++;
#endif /* MSTATS */
#ifdef RCHECK
	/*
	 * Record allocated size of block and
	 * bound space with magic numbers.
	 */
	if (nbytes <= 0x10000)
		p->ov_size = nbytes - 1;
	p->ov_rmagic = RMAGIC;
	*((u_int *)((caddr_t)p + nbytes - RSLOP)) = RMAGIC;
#endif /* RCHECK */
	return ((char *)(p + 1));
}

/*
 * Allocate more memory to the indicated bucket.
 */
static
morecore(bucket)
	register bucket;
{
	register union overhead *op;
	register int rnu;       /* 2^rnu bytes will be requested */
	register int nblks;     /* become nblks blocks of the desired size */
	register int siz;

	if (nextf[bucket])
		return;
	/*
	 * Insure memory is allocated
	 * on a page boundary.  Should
	 * make getpageize call?
	 */
	op = (union overhead *)sbrk(0);
	if ((long)op & 0x3ff)
		sbrk(1024 - ((long)op & 0x3ff));
	/* take 2k unless the block is bigger than that */
	rnu = (bucket <= 8) ? 11 : bucket + 3;
	nblks = 1 << (rnu - (bucket + 3));  /* how many blocks to get */
	if (rnu < bucket)
		rnu = bucket;
	op = (union overhead *)sbrk(1 << rnu);
	/* no more room! */
	if ((long)op == -1)
		return;
	/*
	 * Round up to minimum allocation size boundary
	 * and deduct from block count to reflect.
	 */
	if ((long)op & 7) {
		op = (union overhead *)(((long)op + 8) &~ 7);
		nblks--;
	}
	/*
	 * Add new memory allocated to that on
	 * free list for this hash bucket.
	 */
	nextf[bucket] = op;
	siz = 1 << (bucket + 3);
	while (--nblks > 0) {
		op->ov_next = (union overhead *)((caddr_t)op + siz);
		op = (union overhead *)((caddr_t)op + siz);
	}
}

void
free(cp)
	char *cp;
{   
	register int size;
	register union overhead *op;

	if (cp == NULL || debug > 4)
		return;
	op = (union overhead *)((caddr_t)cp - sizeof (union overhead));
	if (op->ov_magic != MAGIC) {
		Debug("%s free() ignored\n",
		    op->ov_magic == OLDMAGIC ? "Duplicate" : "Bad");
		return;				/* sanity */
	}
	op->ov_magic = OLDMAGIC;
#ifdef RCHECK
	if (op->ov_rmagic != RMAGIC) {
		Debug("Range check failed, free() ignored\n");
		return;
	}
	if (op->ov_index <= 13 &&
	    *(u_int *)((caddr_t)op + op->ov_size + 1 - RSLOP) != RMAGIC) {
		Debug("Range check failed, free() ignored\n");
		return;
	}
#endif /* RCHECK */
	if (op->ov_index >= NBUCKETS)
	    return;
	size = op->ov_index;
	op->ov_next = nextf[size];
	nextf[size] = op;
#ifdef MSTATS
	nmalloc[size]--;
#endif /* MSTATS */
}

/*
 * When a program attempts "storage compaction" as mentioned in the
 * old malloc man page, it realloc's an already freed block.  Usually
 * this is the last block it freed; occasionally it might be farther
 * back.  We have to search all the free lists for the block in order
 * to determine its bucket: 1st we make one pass thru the lists
 * checking only the first block in each; if that fails we search
 * ``reall_srchlen'' blocks in each list for a match (the variable
 * is extern so the caller can modify it).  If that fails we just copy
 * however many bytes was given to realloc() and hope it's not huge.
 */
int reall_srchlen = 4;	/* 4 should be plenty, -1 =>'s whole list */

char *
realloc(cp, nbytes)
	char *cp;
	unsigned nbytes;
{   
	register u_int onb;
	union overhead *op;
	char *res;
	register int i;
	int was_alloced = 0;

	if (cp == NULL)
		return (malloc(nbytes));
	op = (union overhead *)((caddr_t)cp - sizeof (union overhead));
	if (op->ov_magic == MAGIC) {
		was_alloced++;
		i = op->ov_index;
	} else {
		/*
		 * Already free, doing "compaction".
		 *
		 * Search for the old block of memory on the
		 * free list.  First, check the most common
		 * case (last element free'd), then (this failing)
		 * the last ``reall_srchlen'' items free'd.
		 * If all lookups fail, then assume the size of
		 * the memory block being realloc'd is the
		 * smallest possible.
		 */
		if ((i = findbucket(op, 1)) < 0 &&
		    (i = findbucket(op, reall_srchlen)) < 0)
			i = 0;
	}
	onb = (1 << (i + 3)) - sizeof (*op) - RSLOP;
#ifdef RCHECK
	/* There's something wrong with the "onb" size computation, above,
	 * when RCHECK is defined.  If you see this comment and can figure
	 * out exactly how "onb" is being used here, let me know.  Bart.
	 */
	if (was_alloced) {
		free(cp);	/* Hack so there's some chance res == cp */
		was_alloced = 0;
	}
#else /* RCHECK */
	/* avoid the copy if same size block */
	if (was_alloced &&
	    nbytes <= onb && nbytes > (onb >> 1) - sizeof(*op) - RSLOP)
		return(cp);
#endif /* RCHECK */
	if ((res = malloc(nbytes)) == NULL)
		return (NULL);
	if (cp != res)			/* common optimization */
		bcopy(cp, res, (nbytes < onb) ? nbytes : onb);
	if (was_alloced)
		free(cp);
	return (res);
}

/*
 * Search ``srchlen'' elements of each free list for a block whose
 * header starts at ``freep''.  If srchlen is -1 search the whole list.
 * Return bucket number, or -1 if not found.
 */
static
findbucket(freep, srchlen)
	union overhead *freep;
	int srchlen;
{
	register union overhead *p;
	register int i, j;

	for (i = 0; i < NBUCKETS; i++) {
		j = 0;
		for (p = nextf[i]; p && j != srchlen; p = p->ov_next) {
			if (p == freep)
				return (i);
			j++;
		}
	}
	return (-1);
}

#ifdef MSTATS
/*
 * mstats - print out statistics about malloc
 * 
 * Prints two lines of numbers, one showing the length of the free list
 * for each size category, the second showing the number of mallocs -
 * frees for each size category.
 */
mstats(s)
	char *s;
{
	register int i, j;
	register union overhead *p;
	int totfree = 0,
	totused = 0;

	Debug("Memory allocation statistics %s\nfree:\t", s);
	for (i = 0; i < NBUCKETS; i++) {
		for (j = 0, p = nextf[i]; p; p = p->ov_next, j++)
			;
		Debug(" %d", j);
		totfree += j * (1 << (i + 3));
	}
	Debug("\nused:\t");
	for (i = 0; i < NBUCKETS; i++) {
		Debug( " %d", nmalloc[i]);
		totused += nmalloc[i] * (1 << (i + 3));
	}
	Debug("\n\tTotal in use: %d, total free: %d\n",
	    totused, totfree);
}
#endif /* MSTATS */

/* End of modified perl malloc.c */

char *
calloc(nitems, itemsz)
u_int nitems, itemsz;
{
    char *cp;

    cp = malloc(nitems * itemsz);
    bzero(cp, nitems * itemsz);
    return cp;
}

/* These are needed for curses and other external linkage */

#undef free

char *
cfree(p, n, s)
char *p;
u_int n, s;
{
    xfree(p);
    return NULL;
}

char *
free(p)
char *p;
{
    xfree(p);
    return NULL;
}

#else /* INTERNAL_MALLOC */

char *stackbottom;	/* set first thing in main() */

void
xfree(cp)
char *cp;
{
    extern char end[];

    if (cp && cp >= end && cp < stackbottom && cp < (char *) &cp && debug < 5)
	free(cp);
}

#endif /* INTERNAL_MALLOC */

void
free_elems(argv)
char **argv;
{
    register int n;

    if (!argv)
	return;
    for (n = 0; argv[n]; n++)
	xfree(argv[n]);
}

void
free_vec(argv)
char **argv;
{
    free_elems(argv);
    xfree((char *)argv);
}
