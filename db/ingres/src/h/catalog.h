/*
**  CATALOG.H -- system catalog definitions
**
**	relation and attribute catalogs are defined in ingres.h.
**
**	Version:
**		@(#)catalog.h	8.2	5/30/88
*/
#ifndef INGRES_CATALOG_H_
#define INGRES_CATALOG_H_

/* get the definition for USERCODE_SIZE */
#include "gconst.h"

/*
**	INDEX relation struct
**
**	The INDEX relation is used to determine what, if any,
**	secondary indicies exist for relations.  If a relation
**	has a secondary index, there will be one tuple in the
**	INDEX relation for each such index.  There may be one
**	or more domains indexed by one or many INDEX relations
**	depending on whether single or combined indicies are
**	being used.
**
**	Combined indices may use up to six domains to form the
**	index.
**
**	The simple existance of a secondary index is better
**	checked using the "r_indexed" field in the RELATION
**	relation, since that is more efficient.
**
**	The two values SECINDEX and SECBASE are the values for
**	the r_indexed field of the relation relation.  Implicitly
**	SECINDEX must be < 0 and SECBASE must be > 0.
*/

#define	IRELIDP		1
#define	IOWNERP		2
#define	IRELIDI		3

#define	SECINDEX	-2	/* this value in rel.r_indexed indicates
					** that the relation is a sec. index */
#define	SECBASE		1	/* this value in rel.r_indexed indicates
					** has a sec. index */
#define	ORDERED		3	/* this value in rel.r_indexed in 
					** absolute value indicates that the 
					** relation is ordered, if >0 also 
					** indexed */

typedef struct index {
	char	i_relname[MAX_NAME_SIZE]; /*unique name of primary relation */
	char	i_owner[USERCODE_SIZE];	/*owner of primary relation*/
	char	i_index[MAX_NAME_SIZE];	/*unique name of index relation	*/
	char	i_indrelspec;		/*relspec of index relation*/
	char	i_dom[MAX_2ND_KEYS];	/* domain number of primary relation */
			/* which corresponds to each index attribute */
			/* In the indices relation these are stored as */
			/* idom1, idom2, ..,idom6 */
} index_t;



/*
**  TREE RELATION STRUCT
**
**	The TREE relation stores trees used by query modification and
**	for distribution criteria.
*/

struct tree {
	char	treerelid[MAX_NAME_SIZE];	/* relation name */
	char	treeowner[2];		/* relation owner */
	short	treeid;			/* internal name of this tuple */
	short	treeseq;		/* sequence number in tree */
	char	treetype;		/* type info for this tree */
	char	treexxxx;
	char	treetree[100];		/* contents of tree */
};

#define	TREERELID	1
#define	TREEOWNER	2
#define	TREEID		3
#define	TREESEQ		4
#define	TREETYPE	5



/*
**  STRUCT PROTECT -- protection catalog
**
**	This structure defines the format of the 'protect' catalog.
**	One or two things should be noted.  First, the 'p_domset'
**	field is actually four domains in the physical relation,
**	since the best we know about is i4's, and we need an i16.
**	Second, both the p_opset and the p_domset fields
**	are bit maps.
*/

typedef struct protect {
	char	p_rel[MAX_NAME_SIZE];		/* relation to which this applies */
	char	p_owner[USERCODE_SIZE];	/* owner */
	short	p_perm;			/* permission sequence number */
	char	p_user[USERCODE_SIZE];	/* user code in PERMIT */
	char	p_term[8];		/* terminal in PERMIT */
	char	p_result;		/* Resultvarno in tree */
	char	p_opset;		/* operation set */
	short	p_tbegin;		/* beginning time of day */
	short	p_tend;			/* ending time of day */
	char	p_dbegin;		/* beginning day of week */
	char	p_dend;			/* ending day of week */
#ifdef ADDR_ROUNDUP
	short	fill;
#endif
	short	p_domset[8];		/* domain set permitted */
	short	p_tree;			/* link to qualification */
} protect_t;

/* field numbers for find() calls */
#define	PRORELID	1
#define	PRORELOWN	2
#define	PROPERMID	3
#define	PROTREE		16

/* bit values for p_opset */
#define	PRO_RETR	0001	/* retrieve */
#define	PRO_REPL	0002	/* replace */
#define	PRO_DEL		0004	/* delete */
#define	PRO_APP		0010	/* append */
#define	PRO_TEST	0020	/* test in qualification */
#define	PRO_AGGR	0040	/* retrieve aggregate value */



/*
**  STRUCT INTEGRITY -- the descriptor for the integrity relation
*/

struct integrity {
	char	intrelid[MAX_NAME_SIZE];	/* name of the relation */
	char	intrelowner[2];		/* owner of the relation */
	short	inttree;		/* pointer into the tree catalog */
	short	intdomset[8];		/* set of domains this applies to */
	char	intresvar;		/* primary variable number */
};

#define	INTRELID	1
#define	INTRELOWNER	2
#define	INTTREE		3

#endif /* INGRES_CATALOG_H_ */
