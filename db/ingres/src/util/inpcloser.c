#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)inpcloser.c	8.1	12/31/84)

/*
**	inpcloser - close an input relation
**
**	The relation must have been opened by openr with 
**		mode 0 (read only)
**
**	return values:
**		<0 fatal error
**		 0 success
**		 1 relation was not open
**		 2 relation was opened in write mode
**
**	Trace Flags:
**		21.10-11
*/
int
inpcloser(desc_t *d)
{
	register int	i;

#ifdef xATR1
	if (tTf(21, 10))
		printf("inpcloser: %.14s\n", d->d_r.r_id);
#endif
	if (abs(d->d_opened) != (d->d_fd + 1) * 5)
		/* relation not open */
		return (1);

	if (d->d_opened < 0)
		return (2);	/* relation open in write mode */

	i = flush_rel(d, TRUE);	/* flush and reset all pages */

	if (close(d->d_fd))
		i = acc_err(AMCLOSE_ERR);
	d->d_opened = 0;
	return (i);
}
