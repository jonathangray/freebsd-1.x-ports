#include <stdio.h>

#include <ingres.h>
#include <symbol.h>
#include <tree.h>
#include "../decomp/globs.h"
#include "strategy.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)tidtest.c	8.1	12/31/84)

/*
** tid_only_test
** Check the qualification list to see if it
** contains exactly one simple clause, that 
** clause refers to a tid as the VAR, and that
** the binary operation is opEQ.
**
** Side Effects:
**	If the condition holds true, De.ov_hitid and De.ov_lotid
**	are set to refer to the constant value.
** 
** Returns:
**	1 if qualification holds,
**	0 on failure.
**
** Trace Flags:
**	89
**
** Called From:
**	strategy
*/
int
tid_only_test(void)
{
	register sym_t	*c;
	register int		t;
	register sym_t	**q;
	int			found;

	c = (sym_t *) NULL;
#ifdef xOTR1
	if (tTf(89, 0))
		printf("TID_ONLY_TEST\n");
#endif
	found = FALSE;

	q = De.ov_qlist;	/* q holds pointer to qualification */

	if (!q)
		return (0);


	/*
	** iterate through the tree
	*/
	for (t = (*q)->type; t != QLEND; t = (*++q)->type) {
		/*
		** The only thing we allow is a single simple
		** expression with tids.
		*/
		if ( found == TRUE )
			return ( 0 );

		switch (t) {
		  case VAR:
			/*
			** Only allow tids to be vars.
			*/
			if ( (*q)->value.sym_var.attno != 0 )
				return (0);
			t = (*++q)->type;
			if ( t != INT_CONST )
				return ( 0 );
			else {
				c = *q;	/* save pointer to value symbol */
				t = (*++q)->type;
				if (relop(*q, FALSE) == opEQ 
				   && (t = (*++q)->type) == AND) {
					/* found a simple clause */
					found = TRUE;
				}
			}
			break;

		  case INT_CONST:
			c = *q++;
			if ((t = (*q)->type) != VAR)
				return ( 0 );
			else {
				if ( (*q)->value.sym_var.attno != 0 )
					return ( 0 );
				t = (*++q)->type;
				if ( relop(*q, TRUE) == opEQ && (t = (*++q)->type) == AND) {
					/* found a simple clause */
					found = TRUE;
				}
				else
					return ( 0 );
			}

		  default:
			return ( 0 );
		}
	}

#ifdef xOTR1
	if (tTf(89, 2))
		printf("tid_only_test returning %d\n", found);
#endif

	/*
	** We have found a simple clause using only the tid.
	** Set the low and high search keys.
	*/
	if ( found == TRUE ) {
		register	union	symvalue	*p;

		p = &c->value;
		(void) memcpy(&De.ov_lotid, &p->sym_data.i2type, sizeof(De.ov_lotid));
		(void) memcpy(&De.ov_hitid, &p->sym_data.i2type, sizeof(De.ov_hitid));
		dec_tid(&De.ov_lotid);
		return (1);
	}

	return ( 0 );
}/* tid_only_test */

/*
** dec_tid
** Decrement the line-id of a tid
*/
void
dec_tid(tid_t *tid)
{
	tid->line_id--;
}/* dec_tid */
