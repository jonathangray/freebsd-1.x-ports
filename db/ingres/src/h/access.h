/*
**  ACCESS.H -- definitions relating to the access methods.
**
**	Version:
**		@(#)access.h	8.3	5/1/86
*/
#ifndef INGRES_ACCESS_H_
#define INGRES_ACCESS_H_

#include "gconst.h"

/* storage structure flags; < 0 means compressed */
#define	M_HEAP		5		/* paged heap */
#define	M_ISAM		11		/* indexed sequential */
#define	M_HASH		21		/* random hash */
#define	M_BTREE		31		/* BTREES */
#define	M_ORDER		41		/* ordered */
#define	M_TRUNC		99		/* internal pseudo-mode: truncated */
#define M_TYPEOF(a)	(abs(a))
#define M_COMPRESS(a)	(-(a))
#define M_COMPRESSED(a)	((a) < 0)

#define	NACCBUFS	3		/* number of access method buffers */

/* error flags */
#define	AMREAD_ERR	-1
#define	AMWRITE_ERR	-2
#define	AMNOFILE_ERR	-3	/* can't open file for a relation */
#define	AMREL_ERR	-4	/* can't open relation relation */
#define	AMATTR_ERR	-5	/* can't open attribute relation */
#define	AMNOATTS_ERR	-6	/* attribute missing or xtra in att-rel */
#define	AMCLOSE_ERR	-7	/* can't close relation */
#define	AMFIND_ERR	-8	/* unidentifiable stora  Petructure in find */
#define	AMINVL_ERR	-9	/* invalid tid_t */
#define	AMOPNVIEW_ERR	-10	/* attempt to open a view for rd or wr */

/* the following is the access methods buffer */
typedef struct accbuf {
	/* this stuff is actually stored in the relation */
	long		am_mainpg;	/* next main page (0 - eof) */
	long		am_overflowpg;	/* next ovflo page (0 - none) */
	short		am_nextline;	/* next avail line no for this page */
	char		am_tup1[PGSIZE - 12];	/* tuple space */
	short		am_linev[1];	/* line table at buf end - grows down */
			/* am_linev[lineno] is offset into
			** the buffer for that line; am_linev[am_nextline]
			** is free space pointer */

	/* this stuff is not stored in the relation */
	tid_t		am_rel;		/* unique relation id */
	long		am_curpg;	/* page number of the current page */
	int		am_fd;		/* file descriptor for this reln */
	struct accbuf	*am_next;	/* use time link list forward pointer */
	struct accbuf	*am_prev;	/* back pointer */
	int		am_flags;	/* various bits defined below */
#define	BUF_DIRTY	001		/* page has been changed */
#define	BUF_LOCKED	002		/* page has a page lock on it */
#define	BUF_DIRECT	004		/* this is a page from isam direct */
} accbuf_t;

/* pointers to maintain the buffer */
extern accbuf_t	*Acc_head;		/* head of the LRU list */
extern accbuf_t	*Acc_tail;		/* tail of the LRU list */
extern accbuf_t	Acc_buf[NACCBUFS];	/* the buffers themselves */

/*
**  ADMIN file struct
**
**	The ADMIN struct describes the initial part of the ADMIN file
**	which exists in each database.  This file is used to initially
**	create the database, to maintain some information about the
**	database, and to access the RELATION and ATTRIBUTE relations
**	on OPENR calls.
*/

typedef struct adminhdr {
	char	adm_owner[2];	/* user code of data base owner */
	short	adm_flags;	/* database flags */
#define	A_DBCONCUR	0000001	/* set database concurrency */
#define	A_QRYMOD	0000002	/* database uses query modification */
#define	A_NEWFMT	0000004	/* database is post-6.2 */
	short	adm_len;	/* length of adminhdr */
	short	adm_version;	/* database format stamp */
	short	adm_rellen;	/* length of relation descriptor */
	short	adm_attrlen;	/* length of attribute descriptor */
} adminhdr_t;

typedef struct admin {
	adminhdr_t	ad_h;
	desc_t		ad_rel;
	desc_t		ad_attr;
} admin_t;

/* following is buffer space for data from admin file */
extern admin_t		Admin;

/*
**  PGTUPLE -- btree index key (a tid and an index key)
*/

struct pgtuple {
	struct tup_id	childtid;		/* the pointer comes before */
	char		childtup[MAX_TUP_SIZE];
};

/*
** global counters for the number of UNIX read and write
**  requests issued by the access methods.
*/

extern long	Accuread, Accuwrite;

/*
**	Global values used by everything
*/

extern char	*Acctuple;		/* pointer to canonical tuple */
extern		Accerror;		/* error no for fatal errors */
extern char	Accanon[MAX_TUP_SIZE];	/* canonical tuple buffer */


/* Macros for the return values of iutil/add_ovflo.c */
#define	NOBUFFER	-1	/* can't get buffer for overflow page */
#define	NOSETUP		-2	/* can't set up overflow page */
#define	NOGETCURRENT	-3	/* can't get the current page */
#define	NORMVMAIN	-4	/* can't remove the main page */
#define	NOGETOVFLO	-5	/* can't get the overflow page */

#endif /* INGRES_ACCESS_H_ */
