/*
 * ltoza, ltozan - long to zero-padded ascii conversions
 *
 * These functions exist only because there is no portable way
 * to do this with printf and there may be no way do it at all
 * with printf on V7, due to a bug in V7's printf.
 */

#define RADIX 10

/*
 * convert value to at most width characters in outstr, padding with
 * zeros on the left (after any sign); do not terminate with a NUL.
 * returns true iff the value fits in width characters.
 */
int					/* boolean */
ltozan(outstr, value, width)
char *outstr;
long value;
int width;
{
	register char *op = outstr;
	register long wval = value;
	register int wwid = width;

	if (wval < 0 && wwid > 0) {
		*op++ = '-';
		--wwid;
		wval = -wval;		/* fails on smallest int; tough */
	}
	op += wwid - 1;			/* find right end */
	while (wwid-- > 0) {		/* generate "wwid" digits */
		*op-- = wval % RADIX + '0';
		wval /= RADIX;
	}
	return wval == 0;
}

/*
 * convert value to at most width characters in outstr, padding with
 * zeros on the left (after any sign); terminate with a NUL.
 */
int					/* boolean */
ltoza(outstr, value, width)
register char *outstr;			/* char outstr[width+1]; */
long value;
register int width;
{
	register int fits = ltozan(outstr, value, width);

	outstr[width] = '\0';
	return fits;
}
