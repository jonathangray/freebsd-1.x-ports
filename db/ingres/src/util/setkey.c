#include <stdio.h>

#include <ingres.h>
#include <symbol.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)setkey.c	8.1	12/31/84)

/*
**	Clearkeys - reset all key indicators in descriptor
**
**	Clearkeys is used to clear the key supplied
**	flags before calls to setkey
*/

int
clearkeys(desc_t *d)
{
	register int	i;

	for (i = 0; i <= d->d_r.r_attrc; i++)
		d->d_given[i] = 0;
	return (0);
}

/*
**  SETKEY - indicate a partial key for find
**
**	Setkey is used to set a key value into place
**	in a key. The key can be as large as the entire
**	tuple. Setkey moves the key value into the
**	proper place in the key and marks the value as
**	supplied
**
**	If the value is a null pointer, then the key is
**	cleared.
**
**	Clearkeys should be called once before the
**	first call to setkey.
*/
void
ingres_setkey(desc_t *d, void *keyval, void *valueval, int dom)
{
	register int	len;
	char		*cp;
	char		*key;
	char		*value;

	key = (char *) keyval;
	value = (char *) valueval;
#ifdef xATR1
	if (tTf(22, 8))
		printf("setkey: %.14s, %d\n", d->d_r.r_id, dom);
#endif

	/* check validity of domain number */
	if (dom < 1 || dom > d->d_r.r_attrc) {
		syserr("setkey:rel=%12s,dom=%d", d->d_r.r_id, dom);
	}

	/* if value is null, clear key */
	if (value == 0) {
		d->d_given[dom] = 0;
		return;
	}

	/* mark as given */
	d->d_given[dom] = 1;

	len = d->d_len[dom] & I1MASK;
	cp = &key[d->d_off[dom]];

	if (d->d_fmt[dom] == CHAR_CONST) {
		pmove(value, cp, len, ' ');
	} else {
		bmove(value, cp, len);
	}
}
