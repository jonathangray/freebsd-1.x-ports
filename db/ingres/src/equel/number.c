#include <stdio.h>

#include "constants.h"
#include "globals.h"
#include "y.tab.h"
#include "sccs.h"

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)number.c	8.1	12/31/84)


/*
**  NUMBER -- process a numeric token
**	Number gets a number as, either floating or integer,
**	with the QUEL format, from inside a quel statement,
**	and adds it to the symbol space.
**
**	Parameters:
**		chr -- the first character of the number
**
**	Returns:
**		The lexical code for the appropriate
**		type of number.
**
**	Side Effects:
**		Adds a token to the symbols space.
**		yylval is set to the node added.
*/	

int
number(char chr)
{
	double		ftemp;
	long		ltemp;
	int		itemp;
	char		buf [MAXSTRING];
	register char	*ptr;
	register int	ret_type;

	ptr = buf;
	if ((*ptr = chr) != '.') {
		do {
			/* get integer portion */
			if ((ptr - buf) >= MAXSTRING) {
				/* buffer overflow 
				 * return integer 0,
				 * and signal error.
				 */

bufovflo :
				*ptr = '\0';
				yysemerr("numeric too long", buf);
				yylval.u_dn = addsym("0");
				return (Tokens.sp_i2const);
			}

			*++ptr = getch();
		}  while (equel_cmap(*ptr) == NUMBR);
	}

	/* do rest of type determination */
	switch (*ptr) {
	  case '.':
		/* floating point */
		do {
			/* fill into ptr with up to next non-digit */
			if ((ptr - buf) >= MAXSTRING)
				/* buf oflo */
				goto bufovflo;	
			*++ptr = getch();
		}  while (equel_cmap(*ptr) == NUMBR);
		if (*ptr != 'e' && *ptr != 'E') {
			backup(*ptr);
			*ptr = '\0';
			goto convr;
		}

	  case 'e':
	  case 'E':
		if ((ptr - buf) >= MAXSTRING)
			/* buf oflo */
			goto bufovflo;
		*++ptr = getch();
		if (equel_cmap(*ptr) == NUMBR || *ptr == '-' || *ptr == '+') {
			do {
				/* get exponent */
				if ((ptr - buf) >= MAXSTRING)
					/* buf oflo */
					goto bufovflo;
				*++ptr = getch();
			} while (equel_cmap(*ptr) == NUMBR);
		}
		backup(*ptr);
		*ptr = '\0';

convr:
		if (ingres_atof(buf, &ftemp)) {
			/* floating conversion error */
			yysemerr("numeric ofverflow", buf);
			yylval.u_dn = addsym("0");
			return (Tokens.sp_f8const);
		}
		yylval.u_dn = addsym(salloc(buf));
		ret_type = Tokens.sp_f8const;
		break;

	  default:
		/* integer */
		backup(*ptr);
		*ptr = '\0';

		/* long conversion error */
		if (ingres_atol(buf, &ltemp) || ltemp > 32767 
		   || ltemp < -32768)
			goto convr;
		itemp = ltemp;
		yylval.u_dn = addsym(salloc(buf));
		ret_type = Tokens.sp_i2const;
		break;
	}
	return (ret_type);
}
