/*
 * general-purpose in-core hashing, normal interface
 */

typedef char *HASHDATUM;			/* #define won't do due to * */

#ifndef HASHTABLE
#define HASHTABLE struct hashtable
#endif

extern HASHTABLE *hashcreate();
extern hashdestroy(), hashwalk();
extern int hashstore(), hashdelete();
extern HASHDATUM hashfetch(), hashentry();
