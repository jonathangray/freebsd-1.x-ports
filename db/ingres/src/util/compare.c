#include <ingres.h>
#include <access.h>
#include <symbol.h>
#include <aux.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)compare.c	8.1	12/31/84)

int
icompare(char *ax, char *bx, char frmt, char frml)
{
	register ANYTYPE	*a, *b;
	register int		length;
	ANYTYPE			atemp, btemp;

	length = frml & I1MASK;
	if (frmt == CHAR_CONST)
		return (scompare(ax, length, bx, length));
	a = &atemp;
	b = &btemp;
	bmove(ax, (char *) a, length);
	bmove(bx, (char *) b, length);
	if (bequal((char *) a, (char *) b, length))
		return (0);
	switch (frmt) {
	  case INT_CONST:
		switch (length) {
		  case 1:
			return (a->i1type - b->i1type);
		  case 2:
			return (a->i2type - b->i2type);
		  case 4:
			return (a->i4type > b->i4type ? 1 : -1);
		}
		break;

	  case FLOAT_CONST:
		if (frml == 4) {
			if ( a->f4type > b->f4type )
				return ( 1 );
			else
				return ( -1 );
		} else {
			if ( a->f8type > b->f8type )
				return ( 1 );
			else
				return ( -1 );
		}
		break;
	}
	syserr("compare: t=%d,l=%d", frmt, frml);
	/*NOTREACHED*/
	return(-1);
}

/*
**  KCOMPARE -- key compare
**
**	compares all domains indicated by SETKEY in the tuple to the
**	corressponding domains in the key.
**	the comparison is done according to the format of the domain
**	as specified in the descriptor.
**
**	function values:
**		<0 tuple < key
** 		=0 tuple = key
**		>0 tuple > key
*/
int
kcompare (desc_t *dx, void *tupleval, void *keyval)
/* dx - relation descriptor	*/
/* tuple - tuple to be compared	*/
/* key - second tuple or key	*/
{
	register int	i, tmp;
	register desc_t	*d;
	char		*tuple;
	char		*key;

	tuple = (char *) tupleval;
	key = (char *) keyval;

	d = dx;
	for (i = 1; i <= d->d_r.r_attrc; i++) {
		if (d->d_given[i]) {
			if ((tmp = icompare(&tuple[d->d_off[i]],
						&key[d->d_off[i]],
						d->d_fmt[i],
						d->d_len[i])) != 0) {
				return (tmp);
			}
		}
	}
	return (0);
}
