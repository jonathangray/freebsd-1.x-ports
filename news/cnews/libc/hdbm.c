/*
 * general-purpose in-core hashing, dbm interface
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hdbm.h"
#include "hdbmint.h"

/* tunable parameters */
#define RETAIN 300		/* retain & recycle this many HASHENTs */

/* fundamental constants */
#define YES 1
#define NO 0

static HASHENT *hereuse = NULL;
static int reusables = 0;

static unsigned				/* not yet taken modulus table size */
hdbmdef(key)
HDBMDATUM key;
{
	register char *s = key.dat_ptr;
	register unsigned len = key.dat_len;
	register unsigned hash = 0;

	while (len-- > 0)
		hash += *s++;
	return hash;
}

HASHTABLE *
hdbmcreate(size, hashfunc)
register unsigned size;			/* a crude guide to size */
unsigned (*hashfunc)();
{
	register HASHTABLE *tbl;
	register HASHENT **hepp;
	/*
	 * allocate HASHTABLE and (HASHENT *) array together to reduce the
	 * number of malloc calls.  this idiom ensures correct alignment of
	 * the array.
	 * dmr calls the one-element array trick `unwarranted chumminess with
	 * the compiler' though.
	 */
	register struct alignalloc {
		HASHTABLE ht;
		HASHENT *hepa[1];	/* longer than it looks */
	} *aap;

	aap = (struct alignalloc *)
		malloc(sizeof *aap + (size-1)*sizeof(HASHENT *));
	if (aap == NULL)
		return NULL;
	tbl = &aap->ht;
	tbl->ht_size = (size == 0? 1: size);	/* size of 0 is nonsense */
	tbl->ht_magic = HASHMAG;
	tbl->ht_hash = (hashfunc == NULL? hdbmdef: hashfunc);
	tbl->ht_addr = hepp = aap->hepa;
	while (size-- > 0)
		hepp[size] = NULL;
	return tbl;
}

/*
 * free all the memory associated with tbl, erase the pointers to it, and
 * invalidate tbl to prevent further use via other pointers to it.
 */
hdbmdestroy(tbl)
register HASHTABLE *tbl;
{
	register unsigned idx;
	register HASHENT *hp, *next;
	register HASHENT **hepp;
	register int tblsize;

	if (tbl == NULL || BADTBL(tbl))
		return;
	tblsize = tbl->ht_size;
	hepp = tbl->ht_addr;
	for (idx = 0; idx < tblsize; idx++) {
		for (hp = hepp[idx]; hp != NULL; hp = next) {
			next = hp->he_next;
			hp->he_next = NULL;
			hefree(hp);
		}
		hepp[idx] = NULL;
	}
	tbl->ht_magic = 0;		/* de-certify this table */
	tbl->ht_addr = NULL;
	free((char *)tbl);
}

/*
 * The returned value is the address of the pointer that refers to the
 * found object.  Said pointer may be NULL if the object was not found;
 * if so, this pointer should be updated with the address of the object
 * to be inserted, if insertion is desired.
 */
static HASHENT **
hdbmfind(tbl, key)
register HASHTABLE *tbl;
HDBMDATUM key;
{
	register HASHENT *hp, *prevhp = NULL;
	register char *hpkeydat, *keydat = key.dat_ptr;
	register int keylen = key.dat_len;
	register HASHENT **hepp;
	register unsigned size; 

	if (BADTBL(tbl))
		return NULL;
	size = tbl->ht_size;
	if (size == 0)			/* paranoia: avoid division by zero */
		size = 1;
	hepp = &tbl->ht_addr[(*tbl->ht_hash)(key) % size];
	for (hp = *hepp; hp != NULL; prevhp = hp, hp = hp->he_next) {
		hpkeydat = hp->he_key.dat_ptr;
		if (hp->he_key.dat_len == keylen && hpkeydat[0] == keydat[0] &&
		    memcmp(hpkeydat, keydat, keylen) == 0)
			break;
	}
	/* assert: *(returned value) == hp */
	return (prevhp == NULL? hepp: &prevhp->he_next);
}

static HASHENT *
healloc()					/* allocate a hash entry */
{
	register HASHENT *hp;

	if (hereuse == NULL)
		return (HASHENT *)malloc(sizeof(HASHENT));
	/* pull the first reusable one off the pile */
	hp = hereuse;
	hereuse = hereuse->he_next;
	hp->he_next = NULL;			/* prevent accidents */
	reusables--;
	return hp;
}

static
hefree(hp)					/* free a hash entry */
register HASHENT *hp;
{
	if (reusables >= RETAIN)		/* compost heap is full? */
		free((char *)hp);		/* yup, just pitch this one */
	else {					/* no, just stash for reuse */
		++reusables;
		hp->he_next = hereuse;
		hereuse = hp;
	}
}

int
hdbmstore(tbl, key, data)
register HASHTABLE *tbl;
HDBMDATUM key, data;
{
	register HASHENT *hp;
	register HASHENT **nextp;

	if (BADTBL(tbl))
		return NO;
	nextp = hdbmfind(tbl, key);
	if (nextp == NULL)
		return NO;
	hp = *nextp;
	if (hp == NULL) {			/* absent; allocate an entry */
		hp = healloc();
		if (hp == NULL)
			return NO;
		hp->he_next = NULL;
		hp->he_key = key;
		*nextp = hp;			/* append to hash chain */
	}
	hp->he_data = data;	/* supersede any old data for this key */
	return YES;
}

/* return any existing entry for key; otherwise call allocator to make one */
HDBMDATUM
hdbmentry(tbl, key, allocator)
register HASHTABLE *tbl;
HDBMDATUM key;
HDBMDATUM (*allocator)();
{
	register HASHENT *hp;
	register HASHENT **nextp;
	static HDBMDATUM errdatum = { NULL, 0 };

	if (BADTBL(tbl))
		return errdatum;
	nextp = hdbmfind(tbl, key);
	if (nextp == NULL)
		return errdatum;
	hp = *nextp;
	if (hp == NULL) {			/* absent; allocate an entry */
		hp = healloc();
		if (hp == NULL)
			return errdatum;
		hp->he_next = NULL;
		hp->he_key = key;
		hp->he_data = (*allocator)(key);
		*nextp = hp;			/* append to hash chain */
	}
	return hp->he_data;
}

int
hdbmdelete(tbl, key)
register HASHTABLE *tbl;
HDBMDATUM key;
{
	register HASHENT *hp;
	register HASHENT **nextp;

	nextp = hdbmfind(tbl, key);
	if (nextp == NULL)
		return NO;
	hp = *nextp;
	if (hp == NULL)				/* absent */
		return NO;
	*nextp = hp->he_next;			/* skip this entry */
	hp->he_next = NULL;
	hp->he_data.dat_ptr = hp->he_key.dat_ptr = NULL;
	hefree(hp);
	return YES;
}

HDBMDATUM					/* data corresponding to key */
hdbmfetch(tbl, key)
register HASHTABLE *tbl;
HDBMDATUM key;
{
	register HASHENT *hp;
	register HASHENT **nextp;
	static HDBMDATUM errdatum = { NULL, 0 };

	if (BADTBL(tbl))
		return errdatum;
	nextp = hdbmfind(tbl, key);
	if (nextp == NULL)
		return errdatum;
	hp = *nextp;
	if (hp == NULL)				/* absent */
		return errdatum;
	else
		return hp->he_data;
}

/*
 * visit each entry by calling nodefunc at each, with key, data and hook as
 * arguments.  hook is an attempt to allow side-effects and reentrancy at
 * the same time.
 */
hdbmwalk(tbl, nodefunc, hook)
HASHTABLE *tbl;
register int (*nodefunc)();
register char *hook;				/* (void *) really */
{
	register unsigned idx;
	register HASHENT *hp;
	register HASHENT **hepp;
	register int tblsize;

	if (BADTBL(tbl))
		return;
	hepp = tbl->ht_addr;
	tblsize = tbl->ht_size;
	for (idx = 0; idx < tblsize; idx++)
		for (hp = hepp[idx]; hp != NULL; hp = hp->he_next)
			(*nodefunc)(hp->he_key, hp->he_data, hook);
}
