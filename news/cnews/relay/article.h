/*
 * All the information needed to describe an article as it is processed.
 */

#define MINSHPTRS 30		/* initial value for sh_alloced */

struct article {
	statust a_status;	/* article status bits */
	struct headers h;	/* strictly from headers in input */
	char *a_haccum;		/* accumulated output headers, if any */
	char *a_hnext;		/* -> first free byte in a_haccum */
	short a_hpalloced;	/* indices in a_hptrs */
	short a_hpused;		/* indices currently in use */
	char **a_hptrs;		/* -> array of ptrs to lines in a_haccum */
	unsigned a_hbytesleft;	/* in a_haccum */
	char *a_files; /* names for history, added in filing, from h.h_ngs */
	char *a_tmpf;		/* temp link name or first spool dir link */
	FILE *a_artf;		/* stream corresponding to a_tmpf */
	boolean a_unlink;	/* true iff a_tmpf should be unlinked at end */
	boolean a_filed;	/* true iff article has been filed */
	boolean a_xref;		/* true iff Xref: header generated yet */
	boolean a_blvmax;	/* true iff a_unread is to be believed */
	long a_charswritten;	/* into spool directory, for batcher */
	long a_unread;		/* bytes of article input yet unread */
	long a_id;		/* article id #, unique within this batch */
	boolean a_badhdr;	/* true iff non-header is before blank line */
};

/* return name of at least one link, for printing in error messages, etc. */
#define spoolnm(art) ((art)->a_unlink? (art)->a_tmpf: (art)->a_files)

/* imports from article.c */
extern void artinit(), artfree();
