#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <access.h>
#include <catalog.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)closer.c	8.1	12/31/84)

/*
**	closer - close a relation
**
**	CLOSER is used to close a relation which was opened by OPENR.
**	CLOSER should always be called once for each OPENR.
**
**	function values:
**
**		<0  fatal error
**		 0  success
**		 1  relation was not open
*/

int
closer(desc_t *d)
{
	register desc_t	*dx;
	register int	i;

	dx = d;
#ifdef xATR1
	if (tTf(21, 8))
		printf("closer: %.14s,%ld\n", dx->d_r.r_id, dx->d_addc);
#endif

	if ((i = noclose(dx)) != 0)
		return (i);

	flush_rel(dx, TRUE);	/* No error is possible since noclose()
				** has already flushed any pages
				*/

	if (close(dx->d_fd))	/*close the relation*/
		i = acc_err(AMCLOSE_ERR);

	dx->d_opened = 0;

	if (dx->d_r.r_dim > 0) {
		/* close btreesec */
		closer(dx->d_btree);
		close(dx->d_btreefd);
	}
	return (i);
}
