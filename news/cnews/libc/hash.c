/*
 * general-purpose in-core hashing, normal interface (wrapper)
 */

#include <stdio.h>
#include <string.h>
#include "hdbm.h"
#include "hash.h"
#include "hashint.h"

#ifdef notdef
static unsigned				/* not yet taken modulus table size */
hashdef(key)
register HASHDATUM key;
{
	register unsigned hash = 0;
	register char c;

	while ((c = *key++) != '\0')
		hash += c;
	return hash;
}
#endif

HASHTABLE *
hashcreate(size, hashfunc)
unsigned size;				/* a crude guide to size */
unsigned (*hashfunc)();
{
	return hdbmcreate(size, hashfunc);
}

hashdestroy(tbl)
register HASHTABLE *tbl;
{
	hdbmdestroy(tbl);
}

int
hashstore(tbl, key, data)
HASHTABLE *tbl;
register HASHDATUM key, data;
{
	register HDBMDATUM hdbmkey, hdbmdata;

	hdbmkey.dat_ptr = key;			/* actually a string */
	hdbmkey.dat_len = strlen(key);
	hdbmdata.dat_ptr = data;		/* just an address */
	hdbmdata.dat_len = 0;			/* no promises */
	return hdbmstore(tbl, hdbmkey, hdbmdata);
}

static HASHDATUM (*hashalloc)();

static HDBMDATUM
hdbmalloc(key)			/* hdbm->hash->hdbm allocator translator */
HDBMDATUM key;
{
	register HDBMDATUM hdbmdata;

	hdbmdata.dat_ptr = (*hashalloc)(key.dat_ptr);
	hdbmdata.dat_len = 0;			/* just a string */
	return hdbmdata;
}

/* return any existing entry for key; otherwise call allocator to make one */
HASHDATUM
hashentry(tbl, key, allocator)
register HASHTABLE *tbl;
HASHDATUM key;
HASHDATUM (*allocator)();
{
	register HDBMDATUM hdbmkey, hdbmdata;

	hdbmkey.dat_ptr = key;			/* just a string */
	hdbmkey.dat_len = strlen(key);
	hashalloc = allocator;
	hdbmdata = hdbmentry(tbl, hdbmkey, hdbmalloc);
	return hdbmdata.dat_ptr;
}

HASHDATUM					/* data corresponding to key */
hashfetch(tbl, key)
HASHTABLE *tbl;
register HASHDATUM key;
{
	register HDBMDATUM hdbmkey, hdbmdata;

	hdbmkey.dat_ptr = key;			/* actually a string */
	hdbmkey.dat_len = strlen(key);
	hdbmdata = hdbmfetch(tbl, hdbmkey);
	return hdbmdata.dat_ptr;		/* just an address */
}

int
hashdelete(tbl, key)
HASHTABLE *tbl;
register HASHDATUM key;
{
	register HDBMDATUM hdbmkey;

	hdbmkey.dat_ptr = key;			/* actually a string */
	hdbmkey.dat_len = strlen(key);
	return hdbmdelete(tbl, hdbmkey);
}

struct translate {
	char *realhook;
	int (*func)();
};

static
hdbmtohash(key, data, hook)
HDBMDATUM key, data;
char *hook;
{
	register struct translate *thp = (struct translate *)hook;

	(*thp->func)(key.dat_ptr, data.dat_ptr, thp->realhook);
}

/*
 * arrange that at each node, hdbmtohash gets called to map the
 * HDBMDATUM arguments to HASHDATUM arguments.  this also demonstrates
 * how to use the hook argument.
 */
hashwalk(tbl, nodefunc, hook)
HASHTABLE *tbl;
int (*nodefunc)();
char *hook;					/* (void *) really */
{
	struct translate transhook;
	register struct translate *tp = &transhook;

	tp->realhook = hook;
	tp->func = nodefunc;
	hdbmwalk(tbl, hdbmtohash, (char *)tp);
}
