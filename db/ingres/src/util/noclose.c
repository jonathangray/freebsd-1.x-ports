#include <stdio.h>
#include <ingres.h>
#include <aux.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)noclose.c	8.1	12/31/84)

/*
**	noclose - update system catalogs for a relation
**	DESCRIPTION
**
**	function values:
**
**		<0  fatal error
**		 0  success
**		 1  relation was not open
*/

int
noclose(desc_t *d)
{
	register int	i;
	relation_t	rel;

#ifdef xATR1
	if (tTf(21, 12))
		printf("noclose: %.14s,%ld\n", d->d_r.r_id, d->d_addc);
#endif

	/* make sure relation relation is read/write mode */
	if (abs(d->d_opened) != (d->d_fd + 1) * 5)
		return (1);

	/* flush all pages associated with relation */
	/* if system catalog, reset all the buffers so they can't be reused */
	i = flush_rel(d, d->d_r.r_status & S_CATALOG);

	/* check to see if number of tuples has changed */
	if (d->d_addc != 0) {
		/* yes, update the system catalogs */
		/* get tuple from relation relation */
		Admin.ad_rel.d_opened = (Admin.ad_rel.d_fd + 1) * -5;
		if ((i = get_page(&Admin.ad_rel, &d->d_tid)) != 0)
			return (i);	/* fatal error */

		/* get the actual tuple */
		get_tuple(&Admin.ad_rel, &d->d_tid, (char *) &rel);

		/* update the r_tupc field */
		rel.r_tupc += d->d_addc;
		d->d_r.r_tupc = rel.r_tupc;

		/* put the tuple back */
		put_tuple(&d->d_tid, (char *) &rel, Admin.ad_rel.d_r.r_width);
		i = resetacc(Acc_head);
		d->d_addc = 0;
	}
	return (i);
}
