#include <stdio.h>
#include <ingres.h>
#include <symbol.h>
#include "globs.h"
#include "sccs.h"

#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)rangetable.c	8.1	12/31/84)

/*
** Allocation of range table.
** The size of the range table for decomp is
** MAX_RANGES plus 2; 1 for a free aggregate slot and 1 for
** a free secondary index slot.
**
**	Trace Flags:
**		63
*/


void
initrange(void)
{
	register struct rang_tab	*rt;

	for (rt = De.de_rangev; rt <= &De.de_rangev[MAX_RANGES+1]; rt++)
		rt->relnum = -1;
}

/*
**	Save the entry for var in the range table.
*/
void
savrang(int *locrang, int var)
{
	register int	i;

	i = var;
	locrang[i] = De.de_rangev[i].relnum;
}
/*
**	Restore the entry for var from the local range
**	table locrang.
*/
void
rstrang(int *locrang, int var)
{
	register int	i;

	i = var;
	De.de_rangev[i].relnum = locrang[i];
}
/*
**	Update the range name. It is up to
**	the calling routine to openr the new rel.
*/
int
new_range(int var, int relnum)
{
	register int	i, old;

	i = var;

	old = De.de_rangev[i].relnum;
	De.de_rangev[i].relnum = relnum;

	return (old);
}

/*
**	Make a copy of the current range table.
*/
void
newquery(int *locrang)
{
	register struct rang_tab	*rp;
	register int			*ip, i;

	ip = locrang;
	rp = De.de_rangev;

	for (i = 0; i < MAX_RANGES; i++)
		*ip++ = (rp++)->relnum;
}

/*
**	Check the range table to see if any
**	relations changed since the last call
**	to newquery. If so, they were caused
**	by reformat. Restore back the orig relation
**	Reopen it if reopen == TRUE.
*/
void
endquery(int *locrang, int reopen)
{
	register struct rang_tab	*rp;
	register int			*ip, i;
	int				old;
	bool				dstr_flag;

	rp = De.de_rangev;
	ip = locrang;

	dstr_flag = FALSE;
	initp();
	for (i = 0; i < MAX_RANGES; i++) {
		if (rp->relnum != *ip) {
#ifdef xDTR1
			if (tTf(63, -1))
			printf("reformat or reduct changed var %d (%d,%d)\n", i, *ip, rp->relnum);
#endif

			old = new_range(i, *ip);
			dstr_flag |= dstr_mark(old);
			if (reopen)
				openr1(i);
		}

		ip++;
		rp++;
	}

	if (dstr_flag)
		call_dbu(mdDESTROY, FALSE);
	else
		resetp();
}

/*
**	Return the name of the variable "var"
**	in the range table
*/

char *
rangename(int var)
{
	return (rnum_convert(De.de_rangev[var].relnum));
}
