#include <stdio.h>

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)printdesc.c	8.2	1/17/85)

/*
**  PRINT RELATION DESCRIPTOR (for debugging)
**
**	A pointer of a file descriptor is passed.  All pertinent
**	info in that descriptor is printed on the standard output.
**
**	For debugging purposes only
*/
void
printdesc(desc_t *d)
{
	register int	i;
	register int	end;

	printf("Descriptor @ %p %.12s %.2s (%.12s)\n", d,
	    d->d_r.r_id, d->d_r.r_owner, d->d_rangevar);
	printf("spec %d, indxd %d, stat %d, save %s",
		d->d_r.r_spec, d->d_r.r_indexed, d->d_r.r_status,
		ctime(&d->d_r.r_savetime));
	printf("tups %ld, atts %d, wid %d, prim %ld, stamp %s",
		d->d_r.r_tupc, d->d_r.r_attrc, d->d_r.r_width,
		d->d_r.r_primc, ctime(&d->d_r.r_modtime));
	printf("fp %d, opn %d, adds %ld, ",
		d->d_fd, d->d_opened, d->d_addc);
	dumptid(&d->d_tid);

	end = d->d_r.r_attrc;
	for (i = 0; i <= end; i++) {
		printf("[%2d] off %3d fmt %c%3d, xtra %3d, given %3d\n",
			i, d->d_off[i], d->d_fmt[i],
			d->d_len[i] & I1MASK, d->d_iskey[i], d->d_given[i]);
	}
}
