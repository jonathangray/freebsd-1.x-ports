#include <ingres.h>
#include <aux.h>
#include "scanner.h"
#include "sccs.h"
#include <errors.h>

#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)s_number.c	8.3	1/31/86)

/*
** NUMBER
**	scans numerical constants (both integer and floating).  Each
**	constant is converted from ascii to its numerical representation
**	and is entered into the symbol table, indexed by 'yylval'.
**	A token is returned for the number type.
**
**	due to the current atof in the utility library, floating overflow
**	is not checked.
*/
int
number(char chr)
{
	extern int	yylval;
	double		ftemp;
	long		ltemp;
	short		itemp;
	char		buf[256];
	register int	lsave;
	register char	*ptr;

	lsave = Lcase;
	Lcase = 0;
	ptr = buf;
	if ((*ptr = chr) != '.') {
		do {
			/* get integer portion */
			if ((ptr - buf) >= 256) {
				/* buffer overflow */
				par_error(NUMBUFOFLO, WARN, 0, 0, 0);
			}
			*++ptr = get_scan(NORMAL);
		} while (parser_cmap(*ptr) == NUMBR);
	}

	/* do rest of type determination */
	switch (*ptr) {
	  case '.':
		/* floating point */
		do {
			/* fill into ptr with up to next non-digit */
			if ((ptr - buf) >= 256) {
				/* buf oflo */
				par_error(NUMBUFOFLO, WARN, 0, 0, 0);
			}
			*++ptr = get_scan(NORMAL);
		} while (parser_cmap(*ptr) == NUMBR);
		if (*ptr != 'e' && *ptr != 'E') {
			backup(*ptr);
			*ptr = 0;
			goto convr;
		}

	  case 'e':
	  case 'E':
		if ((ptr - buf) >= 256) {
			par_error(NUMBUFOFLO, WARN, 0, 0, 0);	/* buf oflo */
		}
		*++ptr = get_scan(NORMAL);
		if (parser_cmap(*ptr) == NUMBR || *ptr == '-' || *ptr == '+') {
			do {
				/* get exponent */
				if ((ptr - buf) >= 256) {
					/* buf oflo */
					par_error(NUMBUFOFLO, WARN, 0, 0, 0);
				}
				*++ptr = get_scan(NORMAL);
			} while (parser_cmap(*ptr) == NUMBR);
		}
		backup(*ptr);
		*ptr = 0;
		/* FALLTHROUGH */
	convr:
		if (ingres_atof(buf, &ftemp)) {
			/* floating conversion error */
			par_error(FCONSTERR, WARN, buf, 0, 0);
		}
		yylval = (int) syment(&ftemp, 8);
		Lastok.toktyp = Tokens.f8const;
		break;

	  default:
		/* integer */
		backup(*ptr);
		*ptr = 0;
		if (ingres_atol(buf, &ltemp)) {
			/* long conversion error */
			goto convr;
		}
		if (ltemp > 32767) {
			yylval = (int) syment(&ltemp, 4);
			Lastok.toktyp = Tokens.i4const;
			break;
		}
		itemp = ltemp;
		yylval = (int) syment(&itemp, 2);
		Lastok.toktyp = Tokens.i2const;
		break;
	}
	Lcase = lsave;
	Lastok.tok = (char *) yylval;
	Lastok.tokop = 0;
	return (Lastok.toktyp);
}
