#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)add_prim.c	8.1	12/31/84)

/*
**	ADD_PRIM -- Add a primary page to the relation.  Assumes it is to
**		be tacked onto page in current access method buffer.  No
**		disk write is done but the page is marked for writing.
**		It is assumed that the current page in access method buffer
**		is the last physical page in the relation.
**
**	Trace Flags:
**		26.0,2
*/
int
add_prim(desc_t *d, tid_t *tidx)
{
	register accbuf_t	*b;
	register int		i;

	b = Acc_head;
	b->am_mainpg = b->am_curpg + 1;
	b->am_flags |= BUF_DIRTY;
	if ((i = pageflush(b)) != 0) {
		return (i);
	}

	/*
	** Now form the new primary page
	*/

	b->am_curpg = b->am_mainpg;
	b->am_mainpg = 0;
	b->am_overflowpg = 0;
	b->am_linev[0] = (int) b->am_tup1 - (int) b;
	b->am_nextline = 0;
	b->am_flags |= BUF_DIRTY;

	/*
	** Update tid to be new page
	*/
	stuff_page(tidx, &b->am_curpg);
	return (0);
}
