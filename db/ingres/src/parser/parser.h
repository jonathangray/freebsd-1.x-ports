/*
**	COPYRIGHT
**
**	The Regents of the University of California
**
**	1977
**
**	This program material is the property of the
**	Regents of the University of California and
**	may not be reproduced or disclosed without
**	the prior written permission of the owner.
*/

/*
**	Version:
**		@(#)parser.h	8.3	2/8/85
*/
#ifndef INGRES_PARSER_PARSER_H_
#define INGRES_PARSER_PARSER_H_

#define	DBUFSIZ		2000	/* size of buffer for dbu commands */
#define	TREEMAX		2500	/* max number of bytes for tree */
#define	MAXATT		150	/* max number of attributes in the att stash */

#define	V6POINT3COMPAT

#define	WARN		0	/* for a non fatal error */
#define	FATAL		1	/* for a fatal error */

/* mode parameters for range table manipulation */
#define	LOOKREL		1
#define	LOOKVAR		2
#define	R_INTERNAL	3
#define	R_EXTERNAL	4
#define	R_IMPLICIT	5

#define	RELVUSED	01

/* the first argument in argv which may be an ad hoc flag */
#define	FREEFLAGS	6

/* -- ASSORTED DATA STRUCTURES -- */
/* attribute table */
typedef struct atstash {
	char		atbid;			/* attribute number */
	char		atbfrmt;		/* attribute form type */
	char		atbfrml;		/* attribute form length */
	char		atbname[MAX_NAME_SIZE];	/* attribute name */
	struct atstash	*atbnext;		/* pointer to next entry in chain */
} att_ent_t;

/* auxiliary range table */
typedef struct parrng {
	desc_t		vardesc;
	struct parrng	*frontpt;
	struct parrng	*backpt;
	att_ent_t	*attlist;		/* head of attrib list for this reln */
	int		relvused;		/* whether variable in use */
} PARRNG;

/* constant operator lookup table */
struct constop {
	char	*copname;		/* string name for identification */
	int	copnum;			/* op number */
	char	coptype;		/* op result type for formating */
	char	coplen;			/* op result length for formatting */
};

#endif /* INGRES_PARSER_PARSER_H_ */
