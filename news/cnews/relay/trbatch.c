/*
 * transmit batch file management (UUNET-sized-site version; master batch files)
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "hdbm.h"
#include "hash.h"
#include "libc.h"
#include "news.h"
#include "config.h"	/* for artfile() */
#include "msgs.h"
#include "trbatch.h"

/* tunable parameters */
#ifndef MASTERDIR
#define MASTERDIR artfile("out.master")
#endif			/* MASTERDIR */
#ifndef NBFSPERMBF
#define NBFSPERMBF 10	/* # batchfiles per master batchfile (200 for UUNET) */
			/* computable with dup */
#endif			/* NBFSPERMBF */
#ifndef HASHFILSZ
#define HASHFILSZ ((NOPENBFS*NBFSPERMBF)/2)
			/* # of hash buckets for batch file names */
#endif			/* HASHFILSZ */

#define MAXSLOTS (NOPENBFS*NBFSPERMBF)

/* mapping macros */
#define MBORD(slot)  ((unsigned)(slot) / NBFSPERMBF) /* slot to mbf ordinal */
#define MBSLOT(slot) ((unsigned)(slot) % NBFSPERMBF) /* slot to mbf subslot */

/* TODO: dynamically allocate and grow these data structures */
/*
 * Each "batchfile" is actually a master batch file, each of
 * which contains the data needed to create up to NBFSPERMBF batch files
 * (by a separate "exploder" program).  The format is
 *	# optional comment line
 *	<msgid> relative-path size
 *	F out.going/site1/togo
 *	f /usr/spool/news/out.going/site2/togo
 *	n out.nntp/site3
 *	I out.going/site4.wehave/togo
 * where there is a <msgid> line per article and an F, f, n or I line
 * per site getting that article.
 *
 * To avoid splitting multiple sys entries for the same site across
 * master batch files, we need to hash the batch file names down to
 * unique ids, which we then map to a master batch file.
 */
static char ordtombfs[MAXSLOTS];	/* ord -> mb ord map */
static char otomvalid[MAXSLOTS];	/* above mapping valid bits */

struct hashfilename {
	unsigned short hf_mbf;		/* master batch file ordinal # */
};
static HASHTABLE *nmordtbl;		/* name -> mbf ordinal mapping */

/* forwards */
FORWARD struct batchfile *bfinstall();

/*
 * open "name" for appending, for batch sys entry with ordinal # "ord".
 *
 * if ord is too big, see if any batchfile has been assigned to "name" yet;
 * if not, set up a fake batchfile for temporary use.  if ord is in range,
 * ensure that (name, ord) are mapped to a batchfile.
 *
 * if an attempt to open the batchfile's stream fails, close an arbitrary
 * batchfile stream and retry the open.
 */
struct batchfile *
bfopen(name, ord)
register char *name;
register int ord;
{
	register struct batchfile *bf;

	if (ord >= MAXSLOTS) {		/* no mapping possible */
		/* should not get here any more */
		errno = 0;
		errunlock("in bfopen, ord >= MAXSLOTS", "");
#ifdef notdef
		bf = bfisopen(name);
		if (bf == NULL)
			bf = fakebf((FILE *)NULL, name);
#endif
	} else
		bf = bfincache(name, ord);

	if (bf->bf_str == NULL) {
		register char *bfname =
			(bf->bf_name != NULL? bf->bf_name: "fake");

		/* TODO: may want to use popenclex here for exploders */
		bf->bf_str = fopenclex(bfname, "a");	/* silent try */
		if (bf->bf_str == NULL) {
			if (bfrclose() != ST_OKAY)
				return NULL;
			/* retry, may bitch */
			bf->bf_str = fopenwclex(bfname, "a");
		}
		if (bf->bf_str != NULL)
			bfsetup(bf);
	}
	return bf;
}

/*
 * returns a batchfile, never NULL, corresponding to name and ord.
 * if ord isn't mapped, search the batchfile cache for name;
 * If ord & name are unmapped; find free mbf slot, opening new mbf if needed,
 * and map the name and ord.
 */
STATIC struct batchfile *
bfincache(name, ord)
register char *name;
register int ord;			/* batch file ord */
{
	register struct batchfile *mbf;

	if (otomvalid[ord])		/* ord -> mbf mapped? */
		return &batchfile[ordtombfs[ord]];

	mbf = bfisopen(name);
	if (mbf == NULL) {		/* name is unmapped? */
		register int slot, mbord;

		slot = ord;		/* for stability across runs */
		if (slot >= MAXSLOTS) {
			errno = 0;
			errunlock("in bfincache, slot >= MAXSLOTS", "");
		}
		mbord = MBORD(slot);
		mbf = &batchfile[mbord];
		if (mbf->bf_name == NULL) {
			/* open new mbf */
			register char *mbname;
			char uniq[MAXCOMP];
	
			(void) sprintf(uniq, "%d", mbord);
			mbname = str3save(MASTERDIR, SFNDELIM, uniq);
			/* establish new mapping for a new master batch file */
			/* TODO: may want to name an exploder here */
			mbf = bfinstall(mbname, mbord);
			free(mbname);
			ordtobfs[mbord] = mbf;	/* map mbord -> mbf */
		}
	
		/* add this batch file name to the hash chain */
		hfinstall(name, mbord, ord);
	}

	/* set up mapping for the future */
	ordtombfs[ord] = mbf - batchfile;	/* mbord */
	otomvalid[ord] = YES;
	/* mapping is now set (ord -> mbf) */
	return mbf;
}

/* establish new mapping for a new master batch file */
STATIC struct batchfile *
bfinstall(name, ord)
int ord;
char *name;
{
	register struct batchfile *bf = &batchfile[ord];

	if (ordtobfs[ord] != NULL || bf->bf_name != NULL) { /* already in use? */
		errno = 0;
		if (ordtobfs[ord] != NULL)
			errunlock("in bfinstall, ordtobfs[ord] != NULL", "");
		else
			errunlock("in bfinstall, bf->bf_name != NULL", "");
	}
	ordtobfs[ord] = bf;
	bf->bf_name = strsave(name);
	bf->bf_str = NULL;	/* paranoia */
#ifdef notdef
	bf->bf_ref = 0;
#endif
	bf->bf_msgid = NULL;
	bf->bf_lines = FLUSHEVERY;
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

	if (bf->bf_msgid == NULL || !STREQ(bf->bf_msgid, msgid)) {
		/* fresh article on this master batch file */
		if (fflush(bfstr) == EOF)
			return NO;
		nnfree(&bf->bf_msgid);
		bf->bf_msgid = strsave(msgid);

		if (fputs(msgid, bfstr) == EOF)
			return NO;
		(void) putc(' ', bfstr);
		if (fputs(artname, bfstr) == EOF)
			return NO;
		(void) putc(' ', bfstr);
		if (fprintf(bfstr, "%ld", size) == EOF)
			return NO;
		(void) putc('\n', bfstr);
	}

	/* data for this sys entry */
	(void) putc(flag, bfstr);
	(void) putc(' ', bfstr);
#ifdef FULLBATCHFILENAMES
	if (batname[0] != FNDELIM)
		batname = fullartfile(batname);
#endif
	if (fputs(batname, bfstr) == EOF)
		return NO;
	(void) putc('\n', bfstr);
	if (ferror(bfstr))
		return NO;
	return YES;		
}

/* --- hashing --- */

/*
 * search the batchfile cache for "name"; return the corresponding
 * open master batch file, if any.
 */
STATIC struct batchfile *
bfisopen(name)
register char *name;
{
	register struct hashfilename *hf;

	if (nmordtbl == NULL)
		return NULL;
	hf = (struct hashfilename *)hashfetch(nmordtbl, name);
	return (hf == NULL? NULL: &batchfile[hf->hf_mbf]);
}

STATIC
hfinstall(name, mbford, ord)
char *name;
int mbford, ord;
{
	register struct hashfilename *hf;

	if (nmordtbl == NULL)
		nmordtbl = hashcreate(HASHFILSZ, (unsigned (*)())NULL);
	hf = (struct hashfilename *)hashfetch(nmordtbl, name);
	if (hf != NULL)
		return;			/* error: name present */
	/* allocate, append & initialise a new entry */
	hf = (struct hashfilename *)nemalloc(sizeof *hf);
	hf->hf_mbf = mbford;
	if (!hashstore(nmordtbl, strsave(name), (HASHDATUM)hf))
		errunlock("can't store under hash key `%s'", name);
	ordtombfs[ord] = mbford;
	otomvalid[ord] = YES;
}
