/*	
**	BTREE.H -- type definitions and constants for B-Tree structures
**
**	Version :
**		@(#)btree.h	8.1	12/31/84
*/
#ifndef INGRES_BTREE_H_
#define INGRES_BTREE_H_

#include <ingres.h>
#include <access.h>

#define RT		0		/* page number of root */
#define MAXLID		3
#define MAXPTRS		124		/* maximum number of key/ptr pairs in
					** an interior node */
#define MAXLEAVES	82		/* maximum number of tids in a leaf */
#define BTREE		"_SYSbtree"	/* temporary btree file */
#define LIDSIZE		4

typedef struct {
	long	key[MAXPTRS];
	long 	ptr[MAXPTRS];
	char	excess[4];
} Interior;	/* BTree interior node */

typedef struct {
	long	prevleaf;
	long 	nextleaf;
	int	tid_loc[MAXLEAVES];
	int	back_ptr[MAXLEAVES];
	long	tid_pos[MAXLEAVES];
	char	excess[4];
} Leaf;		/* BTree leaf node */

typedef struct BTreeNode {
	short		depth;
	long		prevtree;
	long		nexttree;
	tid_t		prttree;
	char		nodetype;	/* indicates node type */
	int		nelmts;		/* number of elements in a node */
	long 		parent;		/* page number of node's parent */
	union {
		Interior	intnode;
		Leaf		leafnode;
	} node;
} bt_node_t;

typedef struct locator {
	int		offset;		/* offset into array of node values */
	bt_node_t	page;		/* page containing information
					** in node of BTree */
	long		pageno;		/* page number of node */
} locator_t;

long	Prev_lid[MAXLID];
long	Repl_cnt[MAXLID];

#endif /* INGRES_BTREE_H_ */
