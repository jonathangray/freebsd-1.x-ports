/*
 * general-purpose in-core hashing, dbm interface
 */

#define HDBMDATUM struct hdbmdatum
HDBMDATUM {
	char	*dat_ptr;
	unsigned dat_len;
};

#ifndef HASHTABLE
#define HASHTABLE struct hashtable
#endif

extern HASHTABLE *hdbmcreate();
extern hdbmdestroy(), hdbmwalk();
extern int hdbmstore(), hdbmdelete();
extern HDBMDATUM hdbmfetch(), hdbmentry();
