#include <ingres.h>
#include <aux.h>
#include <catalog.h>
#include <access.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)paramd.c	8.2	2/8/85)

/*
** 	GETMODE - get the mode of the storage type	
**
**	Parameters:
**		spec - storage type
**
*/
static int
getmode(int spec)
{
	switch (M_TYPEOF(spec)) {
	  case M_HEAP:
		return(NOKEY);

	  case M_ISAM:
		return(LRANGEKEY);

	  case M_HASH:
		return(EXACTKEY);

	  default: 
		syserr("getmode:bad relspec %d", spec);
	}
	return(NOKEY);
}

/*
**	PARAMD - get access parameters of a relation from its descriptor	
**		and return them in struct pointed to by "ap".
**
**	Parameters:
**		d - descriptor
**		ap - struct which holds the parameters
**
**	Return Codes:
**			0
**
*/
int
paramd(desc_t *d, acc_param_t *ap)
{
	register int	i;
	int		p;


	ap->mode = getmode(d->d_r.r_spec);
	ap->sec_index = FALSE;	/* indicate that this isn't the index-rel */

	for (i = 0; i < MAX_DOMAINS+1; i++)
		ap->keydno[i] = 0;

	for (p = 1; p <= d->d_r.r_attrc; p++)
		if ((i = d->d_iskey[p]) != 0)
			ap->keydno[i-1] = p;
	return (0);
}


/*
**	PARAMI -get indices of a relation from  the index struct
**		and place them in the structure pointed to by param
**
**	Parameters:
**		ip - index struct
**		param - holds info about indices
**
**	Return Codes:
**		0 -- successful
**
*/
int
parami(index_t *ip, acc_param_t *param)
{
	register acc_param_t	*ap;

	ap  = param;
	ap->mode = getmode(ip->i_indrelspec);
	ap->sec_index = TRUE;	/* this is an index */

	bmove(ip->i_dom, ap->keydno, MAX_2ND_KEYS);
	ap->keydno[MAX_2ND_KEYS] = 0;
	return(0);
}
