/*
**	This header file contains all the defined constant
**	and special structures used by decomposition. Certain
**	global variables which are referenced by many modules
**	are also included. By convention global names always
**	begin with a capital letter.
**
**	Version:
**		@(#)decomp.h	8.3	5/1/86
*/
#ifndef INGRES_DECOMP_DECOMP_H_
#define INGRES_DECOMP_DECOMP_H_


#include	<pv.h>


   
#define OVHDP		2		/*  overhead for a projection  */
#define OVHDM		10		/*  overhead for a modify  */

#define MAXRELN	6		/* size of relation descriptor cache */
  
#define QBUFSIZ	2000		/* buffer size (bytes) of original query tree */
#define SQSIZ		10000		/* buffer size for tree copies + sub-queries */
#define AGBUFSIZ	400		/* buffer size for temp agg tree components */
#define PBUFSIZE	500		/* size of parameter buffer area for setp() */
#define PARGSIZE	PV_MAXPC	/* max number of arguments for setp() */

/* symbolic values for GETNXT parameter of fcn GET */
#define NXTTUP	1	/* get next tuple after one specified by tid */

/* flag for no result relation */
#define	NORESULT	-1

/* Range table slot which is always free for aggregate temp rels */
#define	FREEVAR		MAX_RANGES	/* free var number for aggs */

/* Range table slot which is used for secondary index */
#define	SECINDVAR	MAX_RANGES + 1



#define	FIRSTNUM	MAX_RANGES + 3
#define	LASTNUM		100

#endif /* !INGRES_DECOMP_DECOMP_H_ */
