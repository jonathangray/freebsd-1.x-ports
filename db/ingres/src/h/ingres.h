/*
**  INGRES.H -- basic header file for ingres.
**
**	See also aux.h for definitions used by some but not all.
**
**	Version:
**		@(#)ingres.h	8.4	12/8/85
*/
#ifndef INGRES_INGRES_H_
#define INGRES_INGRES_H_

/*
**  Some generally useful stuff.
*/
#include <useful.h>

/* include the global constants */
#include "gconst.h"

/* include tracing information */
#include <trace.h>

/*
**	INGRES manifest constants
**
**	These constants are manifest to the operation of the entire
**	system.  If anything
**	is changed part or all of the system will stop working.
**	The values have been carefully chosen and are not intended
**	to be modifiable by users.
*/

#define	i_1		char
#define	i_2		short
#define	i_4		long

/*
**	RELATION relation struct
**
**	The RELATION relation contains one tuple for each relation
**	in the database.  This relation contains information which
**	describes how each relation is actually stored in the
**	database, who the owner is, information about its size,
**	assorted operation information, etc.
*/

#define	RELID		1	/* domain for ingres_setkey */
#define	RELOWNER	2

typedef struct relation {
	char	r_id[MAX_NAME_SIZE];	/* relation name	*/
	char	r_owner[USERCODE_SIZE];	/* code of relation owner */
	i_1	r_spec;	/* storage mode of relation	*/
				/* M_HEAP  unsorted paged heap	*/
				/* -M_HEAP compressed heap	*/
				/* M_ISAM  isam			*/
				/* -M_ISAM compressed isam	*/
				/* M_HASH  hashed		*/
				/* -M_HASH compressed hash	*/
	i_1	r_indexed;	/* -1 rel is an index, 0 not indexed, 1 indexed */
	i_2	r_status2;	/* more status bits */
	i_2	r_status;	/* relation status bits */
	i_4	r_savetime;	/*unix time until which relation is saved*/
	i_4	r_tupc;	/*number of tuples in relation	*/
	i_2	r_attrc;	/*number of attributes in relation	*/
	i_2	r_width;		/*width (in bytes) of relation	*/
	i_4	r_primc;	/*no. of primary pages in relation*/
	i_4	r_free;	/* head of freelist (b-trees only) */
	i_4	r_modtime;	/* time of last mod*/
	i_2	r_dim;		/* ordering dimension */
} relation_t;


/*
**	ATTRIBUTE relation struct
**
**	The ATTRIBUTE relation contains one tuple for each domain
**	of each relation in the database.  This relation describes
**	the position of each domain in the tuple, its format,
**	its length, and whether or not it is used in part of the key.
*/

#define	ATTRELID	1
#define	ATTOWNER	2
#define	ATTID		3
#define	ATTNAME		4

typedef struct attribute {
	char 	a_rel[MAX_NAME_SIZE];	/*relation name of which this is an attr */
	char	a_owner[USERCODE_SIZE];	/* code of relation owner */
	i_2	a_id;		/*domain number (from 1 to r_attrc)	*/
	char	a_name[MAX_NAME_SIZE];	/*alias for this domain*/
	i_2	a_off;		/*offset in tuple (no. of bytes*/
	i_1	a_fmt;	/* INT_CONST, FLOAT_CONST, CHAR_CONST (in symbol.h) */
	i_1	a_len;	/* unsigned integer no of bytes	*/
	i_1	a_iskey;	/* flag indicating whether this dom is part of a key */
} attr_t;

/*
**	tuple id struct
**
**	We want the line_id to be in the low-order of a long, in
**	order to make index work efficiently; since the order
**	of halfwords is reversed in a VAX, this is dependent...
*/

#include "endian.h"

typedef struct tup_id {
#ifdef LITTLE_ENDIAN  
	char	line_id, pg2, pg1, pg0;
#else
	char	pg0, pg1, pg2, line_id;
#endif
} tid_t;

#include <range.h>		/* to get the descriptor struct */

/* modes to find */
#define	NOKEY		1	/* scan entire relation */
#define	EXACTKEY	2
#define	LRANGEKEY	3	/* low range key */
#define	FULLKEY		4	/* forces full key comparison */
#define	HRANGEKEY	5	/* high range key */
#define	BTREEKEY	6	/* search btree with  exact lid keys */
#define	BTREERANGE	7	/* use btree range to aid search */

/*
**	anytype union -- union of ingres types
*/

typedef union anytype {
	char		i1type;
	short		i2type;
	long		i4type;
	float		f4type;
	double		f8type;
	char		c0type[1];	/* the 1 is bogus, only needs 
					 * starting address
					 */
	char		*cptype;
	char		**cpptype;
} ANYTYPE;

/*
**  Definitions for interface to the control module.
*/

extern char	Qbuf[];

/* structures for user defined delimiters */

#define	BITS		8
#define	ACHARS		128
#define	BITMAPLEN	ACHARS/ BITS


typedef struct dmap {
	int		order;	/* order in bitmap list */
	char		bits[ACHARS];/* 16 8-bit chars, each bits is one ascii char */
	int 		type;	/* 0 = RE_ONE, 1 = RE_ZEROMORE */
	struct dmap 	*next;	/* pointer to next map */
} DMAP;

typedef struct delimlist {
	char			group[MAX_NAME_SIZE];
	char			delim[MAX_NAME_SIZE];
	struct dmap 		*maptr;		/* pointer to map queue */
	struct delimlist	*back;		/* pointer to delim after */
} DELIMLIST;

typedef struct delim_tup {
	int	order;
	char	group[MAX_NAME_SIZE];
	char	delim[MAX_NAME_SIZE];
	int	type;
	char 	bitmap[16];
} DELIM_TUP;

/* structure for saving the header fields */

typedef struct hdrinfo {
	int		len;
	struct hdrinfo 	*next;
} HDRINFO;

/*
**	USERINGRES is the UNIX login name of the INGRES superuser,
**	normally "ingres" of course.  The root of this persons
**	subtree as listed in the password file becomes the root of
**	the INGRES subtree.
*/
#define	USERINGRES	"ingres"

#endif /* !INGRES_INGRES_H_ */
