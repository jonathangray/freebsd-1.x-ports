#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <pv.h>
#include <ingres.h>
#include <aux.h>
#include <batch.h>
#include <access.h>
#include <func.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)resetrel.c	8.2	1/15/85)

extern	short	tTdbu[];

func_t ResetrFn = {
	"RESETREL",
	resetrel,
	null_fn,		/* initialization function */
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};
/*
**	RESETREL -- will change a relation to an empty heap.  This is only
**		to be used on temporary relations and should only be called
**		by the DECOMP process.
*/
int
resetrel(int pc, paramv_t *pv)
{
	extern desc_t	Reldes;
	desc_t		desc;
	char		relname[MAX_NAME_SIZE + 4];
	long		lnum;

	opencatalog("relation", OR_WRITE);
	while (pc-- > 0) {
		if (openr(&desc, OR_RELTID, pv->pv_val.pv_str))
			syserr("RESETREL: openr %s", pv->pv_val.pv_str);
		if (!bequal(Usercode, desc.d_r.r_owner, sizeof(desc.d_r.r_owner)))
			syserr("RESETREL: not owner of %s", pv->pv_val.pv_str);
		ingresname(desc.d_r.r_id, desc.d_r.r_owner, relname);
		if ((desc.d_fd = open(relname, O_CREAT | O_TRUNC | O_WRONLY, FILEMODE)) < 0)
			syserr("RESETREL: create %s", relname);
		lnum = 1;
		if (formatpg(&desc, lnum))
			syserr("RESETREL: formatpg %s", relname);
		desc.d_r.r_tupc = 0;
		desc.d_r.r_spec = M_HEAP;
		desc.d_r.r_primc = 1;
		close(desc.d_fd);
		if (replace(&Reldes, &desc.d_tid, &desc, FALSE) < 0)
			syserr("RESETREL: replace rel %s", relname);
		pv++;
	}
	return (0);

}
