/*
 * All the article header values worth retaining.
 * (strictly from headers in input.)
 *
 * All members of struct headers must point at malloced memory so that
 * freeheaders() can free it without having to keep track of what's
 * malloced and what's static.
 *
 * Furthermore, each member of headers must point at its own private copy
 * of its value string, for the above reason, and no code outside hdr*.c
 * may copy any member nor a modified copy of any member, though it may
 * copy the string pointed to by a (possibly modified) member.
 *
 * Perhaps C++ would allow this to be enforced by a strings class; too bad
 * it's picked up so much Simula baggage (refs, ptui!).  (See section 6.9
 * of The C++ Programming Language for a candidate class.)
 */
struct headers {
	char *h_subj;	/* subject: only needed for controls, -> h_ctlcmd */
	char *h_ngs;	/* newsgroups: used in filing, sys & .ctl matching */
	char *h_distr;	/* distribution for transmit */
	char *h_ctlcmd;	/* control command (NCMP) */
	char *h_etctlcmd;	/* also-control command (NCMP) */
	char *h_approved;	/* acceptance in moderated groups */
	char *h_msgid;	/* history & rejection */
	char *h_artid;	/* history & rejection (obs.) */
	char *h_expiry;	/* history */
	char *h_path;	/* transmit - must munge */
	char *h_sender;	/* transmit in case of moderation */
	char *h_date;	/* reject stale articles */
	char *h_from;	/* merely required */
	char *h_xref;	/* incoming Xref: for -b only; delete on contact */
};

/* common */
extern void hdrdebug(), hdrinit(), freeheaders();

/* munge */
extern void emitxref(), hdrdump(), hdrdigest();

/* parse */
extern void hdrdeflt();
extern boolean ishdr(), contin();
extern char *hdrreq();
