/*
**  Definitions for the range table.
**
**	Version:
**		@(#)range.h	8.2	1/15/85
*/
#ifndef INGRES_RANGE_H_
#define INGRES_RANGE_H_

#include "gconst.h"

/*
**	DESCRIPTOR struct
**
**	The DESCRIPTOR struct is initialized by OPENR to describe any
**	open relation.  The first part of the descriptor is the tuple
**	from the RELATION relation.  The remainder contains some magic
**	numbers and a template initialized from the ATTRIBUTE relation.
**
**	This structure also defines the range table.
*/

typedef struct descriptor {
	relation_t	d_r;
		/*the above part of the descriptor struct is identical
		  to the relation struct and the inormation in this
		  part of the struct is read directly from the
		  relation tuple by openr.  the rest of the descriptor
		  struct is calculated by openr.  */
	char	d_rangevar[MAX_NAME_SIZE];	/* range variable name */
	i_2	d_fd;		/*filep for relation , if open	*/
	i_2	d_opened;		/*indicates if relation is really open*/
	tid_t	 d_tid;	/*when relation is open, this indicates
				  the tid in the relation_t for
				  this relation */
	i_4	d_addc;	/*no. of additions of tuples during this open*/
	i_2	d_off[MAX_DOMAINS];	/*reloff[i] is offset to domain i 	*/
	char	d_fmt[MAX_DOMAINS]; /* format of domain i
				 ** INT_CONST, FLOAT_CONST, or CHAR_CONST  */
	char	d_len[MAX_DOMAINS]; /* d_len[i] is an unsigned integer
				  which indicates length
				  in bytes of domain */
	char	d_iskey[MAX_DOMAINS]; /*relxtra[i] is non-zero if domain i is
				 ** a key domain for the relation */
	char	d_given[MAX_DOMAINS]; /*cleared by openr and set before
				  call to find to indicate value of this
				  domain has been supplied in the key*/
	struct descriptor *d_btree;	/* used to store info about
					secondary btree stucture */
	int	d_btreefd;	/* contains pointer to btree file */
} desc_t;

/*
** Various modes to openr.
*/
#define	OR_READ		0	/* Open relation for reading */
#define	OR_WRITE	2	/* Open relation for writing */
#define	OR_RELTID	-1	/* Only get relation relation 
					** tuple and tid only */
#define	OR_AREAD	-2	/* Open for reading after OR_RELTID */
#define	OR_AWRITE	-3	/* Open for writing after OR_RELTID */
#define	OR_REREAD	-4	/* Open for reading after openr()/closer() */
#define	OR_REWRITE	-5	/* Open for writing after openr()/closer() */


typedef struct {
	desc_t	*rngvdesc;	/* pointer to descriptor for this var */
	bool	rngvmark;	/* send if marked */
} RANGEV;


#endif /* INGRES_RANGE_H_ */
