/*
 * general-purpose in-core hashing, dbm interface (internals)
 */

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

#define BADTBL(tbl)	(((tbl)->ht_magic&BYTEMASK) != HASHMAG)

#define HASHMAG  0257
#define BYTEMASK 0377

#define HASHENT struct hashent

HASHENT {
	HASHENT	*he_next;		/* in hash chain */
	HDBMDATUM he_key;		/* to verify a match */
	HDBMDATUM he_data;
};

HASHTABLE {
	HASHENT **ht_addr;		/* array of HASHENT pointers */
	unsigned ht_size;
	char	ht_magic;
	unsigned (*ht_hash)();
};
