/*
 * fgetfln - read an arbitrarily long line;
 * return a pointer to it, in malloced memory.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <fgetfln.h>
#include <hdbm.h>

#define max(a,b) ((a) > (b)? (a): (b))
#define min(a,b) ((a) < (b)? (a): (b))

#define islimit(lim) ((lim) >= 0)
#define atlimit(n, lim) (islimit(lim) && (n) >= (lim))

/* One could make these arguments, with defaults. */
#define INITLN 90		/* initial allocation per line */
#define GROWLN 200		/* additional allocation size */
#define LARGELN 2048		/* too silly: if was larger, shrink buffer */

#define YES 1
#define NO 0

/* forwards */
static void setup();

struct stream {
	char *line;		/* currently allocated input line */
	unsigned allocn;	/* bytes currently allocated (in line) */
	unsigned maxrd;		/* upper bound on reads into line */
	int incr;		/* for allocn */
	char *segment;		/* start of line segment in "line" */
	char *morep;		/* last byte possibly containing input */
	int lastlen;		/* last line length */
};
static HASHTABLE *streamtab;

/*
 * `fget flagged line' with limit on bytes read.
 * The limit is like fgets's; limit-1 bytes can be read.  -1 means "no limit".
 * The resulting pointer may or may not be malloced and must not be freed.
 * The length of the string is returned via lengthp.
 */
char *
fgetfln(fp, limit, lengthp)
FILE *fp;
register int limit;				/* TODO: make this long? */
int *lengthp;
{
	register int thislen, idx;
	register struct stream *stp;
	HDBMDATUM strkey, strdata;
	static struct stream zstr;

	/*
	 * map fp to internal state for that stream, notably buffer.
	 * Multiple streams can share an fd, so some form of hashing is needed.
	 * One would like this mapping to be very fast.
	 */
	if (streamtab == NULL)
		streamtab = hdbmcreate(30, (unsigned (*)())NULL);
	strkey.dat_ptr = (char *)&fp;
	strkey.dat_len = sizeof fp;
	strdata = hdbmfetch(streamtab, strkey);
	if (strdata.dat_ptr == NULL) {		/* first use of this fp? */
		stp = (struct stream *)malloc(sizeof *stp);
		if (stp == NULL)
			return NULL;
		strdata.dat_ptr = (char *)stp;
		strdata.dat_len = sizeof *stp;
		*stp = zstr;
		stp->allocn = INITLN;
		stp->incr = GROWLN;
		if (hdbmstore(streamtab, strkey, strdata) == 0)
			return NULL;
	}
	stp = (struct stream *)strdata.dat_ptr;

	/* initial allocation for an initial segment of a line */
	if (stp->line == NULL) {
		stp->line = malloc(stp->allocn);
		if (stp->line == NULL)
			return NULL;		/* no memory, can't go on */
	}
	stp->segment = stp->line;
	setup(stp, limit);			/* compute maxrd, morep */

	/* read the first segment of a line */
	*stp->morep = '\0';			/* mark end of segment */
	if (fgets(stp->segment,
	    (int)stp->maxrd - (stp->segment - stp->line), fp) == NULL)
		return NULL;			/* EOF: give up */

	/* read more of this line, if it didn't fit */
	while (*stp->morep != '\0' && *stp->morep != '\n' &&
	    !atlimit(stp->maxrd, limit)) {
		register int oldalloc = stp->allocn;

		/* extend the allocation */
		stp->allocn += stp->incr;
		stp->line = realloc(stp->line, stp->allocn);
		if (stp->line == NULL)
			return NULL;		/* no memory, can't go on */

		/* -1 starts on terminating NUL of the prev. segment */
		stp->segment = stp->line + oldalloc - 1;
		setup(stp, limit);		/* recompute maxrd, morep */

		/* read the next segment */
		*stp->morep = '\0';
		/*
		 * +1 because segment includes terminating NUL of the
		 * prev. segment
		 */
		if (fgets(stp->segment, stp->incr+1, fp) == NULL)
			return NULL;		/* EOF: give up */
	}

	thislen = (stp->segment - stp->line) + strlen(stp->segment);
	if (lengthp != NULL)
		*lengthp = thislen;

	/* contract allocation if worthwhile */
	if (stp->lastlen > LARGELN && thislen < LARGELN) {
		stp->allocn = thislen + 1 + stp->incr;
					/* incr is slop for future lines */
		stp->line = realloc(stp->line, stp->allocn);	/* save space */
	}
	stp->lastlen = thislen;
	return stp->line;
}

static void
setup(stp, limit)				/* compute maxrd, morep */
register struct stream *stp;
register int limit;
{
	stp->maxrd = (islimit(limit)? min(stp->allocn, limit): stp->allocn);
	stp->morep = stp->line + stp->maxrd - 2;
}
