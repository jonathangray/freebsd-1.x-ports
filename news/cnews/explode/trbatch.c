/*
 * transmit batch file management (exploder version)
 */
#include <stdio.h>
#include <sys/types.h>
#include "hdbm.h"
#include "hash.h"
#include "libc.h"
#include "news.h"
/* tunable parameter */
#ifndef NOPENBFS
#define NOPENBFS 250	/* # batchfiles kept open for batching */
#endif			/* NOPENBFS */
#include "trbatch.h"

/* tunable parameters */
#ifndef HASHFILSZ
#define HASHFILSZ (NOPENBFS/2)
			/* # of hash buckets for batch file names */
#endif			/* HASHFILSZ */

static HASHTABLE *nmbftbl;		/* name -> batchfile mapping */

/*
 * open "name" for appending.
 *
 * see if any batchfile has been assigned to "name" yet.
 * if an attempt to open the batchfile's stream fails, close an arbitrary
 * batchfile stream and retry the open.
 */
struct batchfile *
bfopen(name)
register char *name;
{
	register struct batchfile *bf;

	/* malloc_debug(0); */				/* DEBUG */
	bf = bfincache(name);
	if (bf->bf_str == NULL) {
		bf->bf_str = fopenclex(name, "a");	/* silent try */
		if (bf->bf_str == NULL) {
			if (bfrclose() != ST_OKAY)
				return NULL;
			errno = 0;
			warning(
	"had to close a descriptor to reuse it; this should not happen!\n",
				 "");
			/* retry, may bitch */
			bf->bf_str = fopenwclex(name, "a");
		}
		if (bf->bf_str != NULL)
			bfsetup(bf);
	}
	if (bf->bf_str == NULL)
		error("can't open %s", name);
	return bf;
}

/*
 * returns a batchfile, never NULL, corresponding to name.
 */
struct batchfile *
bfincache(name)
char *name;
{
	register struct batchfile *bf;

	bf = bfisopen(name);
	if (bf == NULL) {
		hfinstall(name);
		bf = bfisopen(name);
		if (bf == NULL)
			abort();	/* DEBUG */
	}
	return bf;
}

/*
 * write filename, message-id or size on batch file "bf".
 * under the 'f' flag, include the size in bytes of the article
 * after "name" to assist the C news batcher.  under the 'n' flag,
 * write the article's message-id.  afterward, flush "bf" in case
 * the machine crashes before the stream is closed.
 * don't check putc return value for portability; use ferror.
 */
int							/* boolean */
bfappend(bf, flag, batname, artname, msgid, size)
register struct batchfile *bf;
int flag;
char *batname, *artname, *msgid;
long size;
{
	register FILE *bfstr = bf->bf_str;

	if (flag == 'I')
		artname = msgid;			/* cheat */
	if (fputs(artname, bfstr) == EOF)
		return NO;
	if (flag == 'f' && fprintf(bfstr, " %ld", size) == EOF)
		return NO;
	if (flag == 'n') {
		(void) putc(' ', bfstr);
		if (ferror(bfstr) || fputs(msgid, bfstr) == EOF)
			return NO;
	}
	(void) putc('\n', bfstr);
	if (ferror(bfstr) || bfflush(bf) == EOF)
		return NO;
	return YES;		
}

/* --- hashing --- */

struct closehook {
	short	closedone;
	statust	status;
};

STATIC int
closefirst(key, data, hook)
HASHDATUM key, data;
char *hook;
{
	register struct closehook *chp = (struct closehook *)hook;
	register struct batchfile *bf;

	if (chp->closedone)
		return;
	bf = (struct batchfile *)data;
	if (bf->bf_str == NULL)
		return;
	chp->status = bfclose(bf);
	chp->closedone = YES;
}

STATIC statust
bfrclose()				/* close an arbitrary batchfile */
{
	struct closehook closehook;
	register struct closehook *chp = &closehook;

	chp->closedone = NO;
	chp->status = ST_OKAY;
	if (nmbftbl == NULL)
		return chp->status;
	hashwalk(nmbftbl, closefirst, (char *)chp);
	if (!chp->closedone) {
		errno = 0;
		warning("bfrclose couldn't find an open descriptor to close",
			"");
	}
	return chp->status;
}

STATIC int
closeone(key, data, hook)		/* close a given open batch file */
HASHDATUM key, data;
char *hook;
{
	register struct closehook *chp = (struct closehook *)hook;
	register struct batchfile *bf = (struct batchfile *)data;

	if (bf->bf_str != NULL)		/* batch file stream open */
		chp->status |= bfclose(bf);
#ifdef notdef
	bf->bf_ref = 0;
#endif
	if (!hashdelete(nmbftbl, key))
		error("can't delete hash key `%s'", key);
}

STATIC statust
bfrealclose()				/* close all open batch files */
{
	struct closehook closehook;
	register struct closehook *chp = &closehook;

	chp->status = ST_OKAY;
	if (nmbftbl != NULL)
		hashwalk(nmbftbl, closeone, (char *)chp);
	return chp->status;
}

STATIC
hfinstall(name)
char *name;
{
	register struct batchfile *bf;

	if (nmbftbl == NULL)
		nmbftbl = hashcreate(HASHFILSZ, (unsigned (*)())NULL);
	bf = (struct batchfile *)hashfetch(nmbftbl, name);
	if (bf != NULL)
		return;			/* error: name present */
	/* allocate, append & initialise a new entry */
	bf = (struct batchfile *)nemalloc(sizeof *bf);
	(void) memset((char *)bf, 0, sizeof *bf);
	bf->bf_name = strsave(name);	/* NEEDED? */
	bf->bf_str = NULL;	/* paranoia */
#ifdef notdef
	bf->bf_ref = 0;
#endif
	bf->bf_lines = FLUSHEVERY;
	if (!hashstore(nmbftbl, bf->bf_name, (HASHDATUM)bf))
		error("can't store under hash key `%s'", name);
}

/*
 * search the batchfile cache for "name"; return the corresponding
 * open master batch file, if any.
 */
STATIC struct batchfile *
bfisopen(name)
char *name;
{
	return nmbftbl == NULL? NULL:
		(struct batchfile *)hashfetch(nmbftbl, name);
}
