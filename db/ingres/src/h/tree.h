/*
**  TREE.H -- defines the structure of a querytree
**
**	Version:
**		@(#)tree.h	8.2	4/13/85
*/
#ifndef INGRES_TREE_H_
#define INGRES_TREE_H_

#include <ingres.h>

/*
**	Structures Used In The Value Fields of Querytree nodes
*/

/*
**  STRKEEPER
**
**	This stores interval information for a char attribute
**	type -- OPEN vs. CLOSED intervals
**	strings -- the two delimiters
**	flag -- 1st bit set if it is a special character
**		2nd bit set if backwards searching indicated
**      number -- which occurence of strings to grab
**
*/
typedef struct strkeeper {
	char	type[2];
	char	*string[2];
	char	flag[2];
	char	number[2];
} STRKEEPER;

/*
**  VAR node
**
**	This node type contains info for a tuple variable.
**	varno -- index into range table
**	attno -- attribute number in this relation
**	varfrmt -- type of this domain
**	varfrml -- length of this domain
**	valptr -- pointer to value when bound.
**
**	If varno == -1, then this variable has been substituted; to
**	get the actual VAR node, follow the chain of valptr's.
*/
typedef struct varnode {
	char		varno;		/* variable number */
	char		attno;		/* attribute number */
	char		varfrmt;	/* type */
	char		varfrml;	/* length */
	STRKEEPER 	*varstr;	/* pointer to delimiter */
	ANYTYPE		*valptr;	/* pointer to value */
} varnode_t;

/*
**	STRUCTURE FOR AND, AGHEAD, AND ROOT NODES
**
**		In the parser and qrymod none of these fields are used.
**		Decomp maintains information about the variables
**		in the left and right descendents of the nodes.
**		The "rootuser" flag is present only in the ROOT and AGHEAD
**		nodes.  It is TRUE only in the original ROOT node of the query.
*/

/* AND, AGHEAD, ROOT nodes */
typedef struct rootnode {
	char	tvarc;		/* total of var's in sub-tree */
	char	lvarc;		/* # of variables in left branch */
	short	lvarm;		/* bit map of var's in left branch */
	short	rvarm;		/* bit map of var's in right branch*/
	short	rootuser;	/* flag: TRUE if root of user generated query */
} rootnode_t;

/* AOP, BOP, UOP nodes */
typedef struct opnode {
	short	opno;		/* operator number */
	char	opfrmt;		/* format of function */
	char	opfrml;		/* length of function */
	char	agfrmt;		/* in AOP, format of result */
	char	agfrml;		/* in AOP, length of result */
} opnode_t;

/* RESDOM node */
typedef struct resdomnode {
	short	resno;		/* result domain number */
	char	resfrmt;	/* result format */
	char	resfrml;	/* result length */
} resdomnode_t;

/* SOURCEID node */
typedef struct srcid {
	short	srcvar;			/* variable number */
	desc_t	srcdesc;		/* descriptor for this var */
} srcid_t;

/*
**	SYMVALUE UNION
**
**		This union contains all of the types available
**		in the value field of a querytree node.
*/

union symvalue {
	union anytype		sym_data;
	varnode_t		sym_var;
	rootnode_t		sym_root;
	opnode_t		sym_op;
	resdomnode_t		sym_resdom;
	srcid_t			sym_srcid;
};


/*
**	SYMBOL DEFINITION
**
**		Basic symbol structure. "Type" is one of the symbols
**		in "symbol.h", "len" is the length of the "value"
**		field (0 to MAX_FIELD_SIZE bytes), "value" is variable length and
**		holds the actual value (if len != 0) of the node.
**		The "value" is one of the types contained in "union symvalue".
**
**		On a thirty-two bit machine, there are two bytes of padding
**		after type and length.  These two bytes are discarded 
**		when a symbol is written to a pipe.
**
**		SYMOFF should be set to the number of bytes between
**		the start to this structure and where the value field starts
*/

#define	SYMOFF	4

typedef struct symbol {
	char		type;		/* type codes in symbol.h */
	char		len;		/* length in bytes of value field */
	union symvalue	value;
	int		start;		/* start of string if char */
} sym_t;

/*
**	QUERYTREE NODE
**
**		Basic node in the querytree. Each node has a left and
**		right descendent. If the node is a leaf node then the
**		left and right pointers will be NULL. Depending on the
**		"type" field of the symbol structure, there may be additional
**		information.
*/

typedef struct querytree {
	struct querytree	*left;
	struct querytree	*right;
	sym_t			sym;
} qtree_t;

/*
**	SUNDRY CONSTANTS
**
**		There are several differences in the handling of data
**		structures on 16 and 32 bit machines:
**			1).  A pointer to  memory is either 2 or 4 bytes.
**			2).  Padding is inserted in structures to insure
**				alignment of 16 and 32 bit numbers.
**
**		For these reasons the following constant definitions
**		are useful for machine independent allocation.
**
**		These are based on the PDP11 compile flag.
**
**		QT_HDR_SIZ -- size of left and right pointers, typ,
**			len and padding
**		SYM_HDR_SIZ -- size of type and len in symbol
**			structure -- includes any padding before
**			the value field
**		TYP_LEN_SIZ -- size of type and len in symbol
**			structure -- without padding
**
**		INGRES FOLKS: don't change these back to sizeof's!!!
**			      The PDP-11 compiler doesn't understand!
*/

/* XXX - agc */
#define	QT_HDR_SIZ	12
#define	SYM_HDR_SIZ	4
#define	TYP_LEN_SIZ	2

/*
**  Query Tree Header.
**
**	Qt_ctx should be of type 'ctx_t *', but it is 'char *'
**		to insure that we don't need ctlmod.h.
*/

struct qthdr {
	char	qt_active;		/* if set, Qt area is in use */
	char	*qt_ctx;		/* pointer to context */
	short	qt_qmode;		/* query mode */
	short	qt_resvar;		/* result variable number */
	RANGEV	qt_rangev[MAX_RANGES];	/* the range table */
	short	qt_remap[MAX_RANGES];	/* variable remapping (for QM) */
} Qt;

#endif /* !INGRES_TREE_H_ */
