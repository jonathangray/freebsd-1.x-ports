/*
 * parsed form of the "sys" file
 * Beware that in C++, struct system collides with system(3) in transmit.c
 * This can be fixed by using "::system(...)" or by renaming struct system.
 */
struct system {
	char *sy_name;		/* machine name */
	char *sy_excl;		/* exclusion list of machines */
	char *sy_ngs;		/* newsgroup subscription list */
	char *sy_distr;		/* distribution list */
	char *sy_cmd;		/* command to transmit articles */
	unsigned sy_lochops;	/* flags Ln value: local hops */
	char sy_flags;		/* ornaments, encoded as bits */
	NGPAT *sy_trngs;	/* parsed form of sy_ngs */
	NGPAT *sy_trdistr;	/* parsed form of sy_trdistr */
	struct system *sy_next;	/* link to next system */
};

/* sy_flags bits */
#define FLG_BATCH	(1<<0)		/* F: sy_cmd is batch filename */
#define FLG_SZBATCH	(1<<1)		/* f: F, and include byte count */
#define FLG_IHAVE	(1<<2)		/* I: NNTP ihave - F, write msg. ids */
#define FLG_LOCAL	(1<<3)		/* L: send local articles only */
#define FLG_MOD		(1<<4)		/* m: send moderated groups only */
#define FLG_UNMOD	(1<<5)		/* u: send unmoderated groups only */
#define FLG_NBATCH	(1<<6)		/* n: NNTP batch: filename & msg-id */

/* imports from system.c */
extern struct system *oursys(), *nextsys();
extern void sysdeflt(), rewndsys();
