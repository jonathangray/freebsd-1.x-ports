#ifndef INGRES_DECOMP_GLOBS_H_
#define INGRES_DECOMP_GLOBS_H_

#include <tree.h>
#include <func.h>
#include <access.h>
#include "../decomp/decomp.h"
#include "../ovqp/ovqp.h"

/*	@(#)globs.h	8.1	12/31/84	*/


struct agglist {
	qtree_t	**father;	/* addr of pointer to you */
	qtree_t	*agpoint;	/* pointer to aghead */
	qtree_t	*agfather;	/* is your father an aggregate? */
	short	agvarno;	/* var # assigned to aggr fnct */
};

struct hitlist {
	qtree_t	**trepr;	/* position in tree to be changed */
	short	byno;		/* by-list position */
};

struct rang_tab {
	int		relnum;		/* internal decomp relation number */
	int		rtspec;		/* relspec of relation */
	int		rtstat;		/* relstat of relation */
	int		rtwid;		/* relwidth of relation */
	long		rtcnt;		/* tupcount of relation */
	int		rtaltnum;	/* reserved for distributed decomp */
	char		*rtattmap;	/* reserved for distributed decomp */
	long		rtdcnt;		/* reserved for distributed decomp */
	struct d_range	*rtsrange;	/* reserved for distributed decomp */
};


/* The following structure reserved for distributed decomp */
/* The order of this has been changed for proper alignment */
struct d_range {
	int		relnum;
	int		draltnum;
	long		drtupcnt;
	struct d_range	*drnext;
	int		drstat;
	char		drsite[2];
};


/* structure used by reduction to maintain component pieces */
typedef struct complist {
	struct complist	*nextcomp;	/* next comp piece */
	struct complist	*linkcomp;	/* next clause of this comp */
	qtree_t		*clause;	/* associated clause */
	int		bitmap;		/* map of all assoc clauses */
} comp_list_t;

bool	Batchupd;

struct desc_tab {
	int	relnum;		/* relation number */
	char	dtmode;		/* status of descriptor */
	char	dtpos;		/* position of last access */
	desc_t	desc;		/* descriptor */
};


struct stacksym {
	char	s_type;
	char	s_len;
	long	s_value[2];
};				/* stack for OVQP interpreter */


typedef struct simp {
	int	relop;	/* value of relop in simp clause*/
	int	att;	/* attno of attribute */
        sym_t  *constv; /* pointer to constant value symbol */
} simp_t;


typedef struct key {
	sym_t	*keysym;
	int	dnumber;
} key_t;

extern int	Equel;

struct {
		/* OVQP variables */
	char		ov_outtup[MAX_TUP_SIZE];
	char		ov_intup[MAX_TUP_SIZE];
	char		*ov_origtup;
	char		*ov_tend;	/* pts to end of data in ov_outtup */
	short		ov_bopen;	/* TRUE if batch file is open */
	short		ov_targvc;	/* var count in Target list (flags constant Targ. list) */
	short		ov_qualvc;	/* var count in Qual list */
	short		ov_userqry;	/* flags a query on the users's result rel */
	short		ov_retrieve;	/* true is a retrieve, else false */
	char		*ov_ovqpbuf;
	short		ov_diffrel;	/* true is ov_source and ov_result are different */
	short		ov_agcount;	/* count of the # of aggregates in the query */
	short		ov_qvpointer;
	long		ov_intid;
	tid_t		ov_uptid;
	long		*ov_counter;	/* cnts "gets" done in OVQP */
	tid_t		ov_lotid;
	tid_t		ov_hitid;	/* lo & hi limits of scan in OVQP */
	long		ov_tupsfound;	/* counts # tuples which satified the query */
	desc_t		*ov_scanr;	/* pts to desc of reln to be scanned */
	desc_t		*ov_source;	/* 0 if no source for qry, else poshorts to ov_srcdesc */
	desc_t		*ov_result;	/* 0 if no result for qry, else poshorts to ov_reldesc */
	sym_t		**ov_tlist;	/* Target LIST */
	sym_t		**ov_alist;	/* Aggregate LIST */
	sym_t		**ov_qlist;	/* Query List */
	sym_t		**ov_bylist;	/* By List */
	struct stacksym	ov_stack[MAX_STACK_SIZE];
	char		ov_keyl[MAX_TUP_SIZE];
	char		ov_keyh[MAX_TUP_SIZE];
	int		ov_nsimp;	/* Current no. entries in ov_simp vector */
	int		ov_fmode;	/* find-mode determined by strategy */
	simp_t	ov_simp[NSIMP];
	key_t	ov_lkey_struct[MAX_DOMAINS+1];
	key_t	ov_hkey_struct[MAX_DOMAINS+1];

		/* DECOMP/OVQP variables */

	struct agglist	*de_aggnext;	/* next in aggregate list */
	struct agglist	*de_agglim;	/* limit in aggregate list */
	struct hitlist	*de_hnext;
	struct hitlist	*de_hlimit;
	char		de_d_dbu70;
	char		de_d_ovqp70;
	int		de_synconly;
	int		de_error_flag;
	int		de_qvptr;		/* index into available de_qvect space in ovqpnod() */
	sym_t		*de_qvect[MAXNODES];
	short		de_newq;		/* OVPQ must devise new strategy */
	short		de_newr;		/* force OVQP to reopen result relation */
	int		de_qmode;		/* flag set to indicate mode of tuple disposition */
	int		de_resultvar;		/* if >= 0 result variable */
	int		de_sourcevar;		/* likewise for source variable */
	char		*de_qbuf;		/* pointer to query buffer */



	qtree_t		*de_qle;		/* ptr to QLEND node */
	qtree_t		*de_tr;			/* ptr to TREE node */
	int		de_dfiles;		/* number of available file descriptors */
	int		de_dopnfiles;		/* Number of open file descriptors */
	struct desc_tab	de_desc[MAXRELN];	/* descriptors available for use */
	struct rang_tab	de_rangev[MAX_RANGES+2];	/* global range table with extra slot for FREEVAR and SECINDVAR */
	int		de_qry_mode;		/* mode of original query (not nec same as de_qmode) */
	char		de_name_table[FIRSTNUM-1][MAX_NAME_SIZE];
	char		de_num_used[LASTNUM+1];
	char		de_buflag;
} De;

short		tTdecomp[100];

#ifdef tTf
#undef tTf
#endif tTf
#define ABS(val)	(((val) < 0) ? -(val) : (val))
#define tTf(a, b)	((b < 0) ? tTdecomp[a] : (tTdecomp[a] & (1 << ABS(b))))

#endif /* !INGRES_DECOMP_GLOBS_H_ */
