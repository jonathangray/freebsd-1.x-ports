#include <ingres.h>
#include <symbol.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIconvert.c	8.1	12/31/84)


/*
**
**	IIconvert -- Equel run-tme routine to convert
**		numeric values of one type and length, to a
**		(not necessarily) different type and length.
**
**		The source numeric can be i1, i2, i4, f4, or f8.
**
**		The source number will be converted to the
**		type and length specified in the destination.
**		It also must be one of i1, i2, i4, f4, or f8.
**
**	Returns:
**		IIconvert returns 0 if no overflow occured,
**		otherwise it returns -1
*/

int
IIconvert(char *inp, char *outp, int sf, int slen, int df, int dlen)
/* inp - input area */
/* outp - output area */
/* sf - format of the source number */
/* slen - length of the source number */
/* df - format of the dest */
/* dlen - length of the dest */
{
	char			number[8];	/* dummy buffer */
	register ANYTYPE	*num;
	register int		sl;
	register int		dl;

	dl = dlen;
	sl = slen;
	num = (ANYTYPE *) number;
	IIbmove(inp, num,  sl);	/* copy number into buffer */

	if (sf != df) {
		/* if the source and destination formats are
		 * different then the source must be converted
		 * to i4 if the dest is int, otherwise to f8 
		 */

		if (df == FLOAT_CONST)	/* {sf == INT_CONST} INT_CONST->f8 */ {
			switch (sl) {

			  case 1:
				num->f8type = num->i1type;	/* i1 to f8 */
				break;

			  case 2:
				num->f8type = num->i2type;	/* i2 to f8 */
				break;

			  case 4:
				num->f8type = num->i4type;	/* i4 to f8 */
			}
			sl = 8;			/* df == INT_CONST && sf == FLOAT_CONST 
						 * && sl == 8
						 */
		} else {
			/* {df == INT_CONST && sf == FLOAT_CONST} FLOAT_CONST->i4 */

			/* check if float >  2**31 */
			if (sl == 8)
				num->f4type = num->f8type;	/* convert f8 to f4 */

			if (num->f4type > 2147483647.0 || num->f4type < -2147483648.0)
				return (-1);
			num->i4type = num->f4type;
			sl = 4;
		}
	}

	/* number is now the same type as destination */
	/* convert lengths to match */

	if (sl != dl) {
		/* lengths don't match. convert. */
		if (df == FLOAT_CONST) {
			if (dl == 8)
				num->f8type = num->f4type;	/* f4 to f8 */
			else
				num->f4type = num->f8type;	/* f8 to f4 with rounding */
		} else {
			switch (dl) {

			  case 1:
				if (sl == 2)		/* i2 to i1 */ {
					if (num->i2type > 127 || num->i2type < -128)
						return (-1);
					num->i1type = num->i2type;	
				} else			/* i4 to i1 */ {
					if (num->i4type > 127 || num->i4type < -128)
						return (-1);
					num->i1type = num->i4type;
				}
				break;

			  case 2:
				if (sl == 1)		/* i1 to i2 */ {
					num->i2type = num->i1type;	
				} else			/* i4 to i2 */ {
					if (num->i4type > 32767 || num->i4type <-32768)
						return (-1);
					num->i2type = num->i4type;
				}
				break;

			  case 4:
				if (sl == 1)		/* i1 to i4 */
					num->i4type = num->i1type;	
				else			/* i2 to i4 */
					num->i4type = num->i2type;
			}
		}
	}

	/* conversion is complete 
	 * copy the result into outp
	 */

	IIbmove(num, outp, dl);
	return (0);
}
