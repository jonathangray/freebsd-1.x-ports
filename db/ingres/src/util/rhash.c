#include <stdio.h>

#include <ingres.h>
#include <aux.h>
#include <symbol.h>
#include <access.h>
#include <lock.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)rhash.c	8.2	1/17/85)

/*
**  RHASH -- perform a randomizing hash on the full key.
**
**	Trace Flags:
**		26.12-13
*/

/*
** ADDABYTE is used to map a long key into a four byte integer.
** As bytes are added, they are first rotated, then exclusive ored
** into the existing key.
*/
static void
addabyte(char ch, long *word, int knt1)
{
	register int	knt;
	long		i;

	knt = knt1;
	i = ch & I1MASK;	/*get rid of any sign extension*/
	knt += 8 * (knt & 3);	/*alternately add 0, 8, 16 or 24 to knt */
	knt &= 037;
	*word ^= (i << (knt) | i >> (32 - knt));
}

long
rhash(register desc_t *d, char *key)
{
	register int	i;
	register char	*cp;
	long		bucket;
	long		*k;
	char		tmp;
	int		j;
	int		knt;
	int		numeric;

	bucket = 0;
	knt = 0;
	for (i = 1; i <= d->d_r.r_attrc; i++)
		if (d->d_iskey[i]) {
			/* form pointer to field */
			cp = &key[d->d_off[i]];
			numeric = (d->d_fmt[i] != CHAR_CONST);
			for (j = 0; j < (d->d_len[i] & I1MASK); j++)
				if (((tmp = *cp++) != ' ') || numeric)
					addabyte(tmp, &bucket, knt++);
		}
	/* remove sign bit from bucket the hard way */
	k = &bucket;
	*k &= 077777;
#ifdef xATR3
	if (tTf(19, 12))
		printf("rhash:hval=%ld", bucket);
#endif
	bucket %= d->d_r.r_primc;
#ifdef xATR3
	if (tTf(19, 12))
		printf(",returning %ld\n", bucket);
#endif
	return (bucket);
}
