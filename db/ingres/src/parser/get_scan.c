#include <ctype.h>
#include <ingres.h>
#include "scanner.h"
#include "sccs.h"

#define INGRES_GUTIL
#define INGRES_CTLMOD
#include "protos.h"

SCCSID(@(#)get_scan.c	8.2	1/17/85)

/*
** GET_SCAN -- gets characters from monitor
**
**	Parameters:
**		mode --
**	   	    modes are:
**			NORMAL = read normally
**			PRIME = prime the pipe
**			SYNC = sync (or flush) the pipe
**
**	Returns:
**		character or '\0' on eof
**
**	Trace Flags:
**		Getscan ~~ 54.0
*/
int
get_scan(int mode)
{
	extern int		yyline;
	register int		ctr;
	char			c;

	extern int		Pctr;		/* vble for backup stack in scanner */
	extern char		Pchar[2];

	ctr = 0;
#ifdef	xPTR3
	tTfp(54, 0, "get_scan: mode %d ", mode);
#endif

	switch (mode) {
	    case NORMAL:
		if (Pctr) {
			c = Pchar[--Pctr];
			ctr = 1;
		} else
			ctr = readmon(&c, 1);
		if (c == '\n')
			yyline++;
		if (Lcase && isupper(c)) {
			c = tolower(c);
		}
		break;

	    case PRIME:
		Pctr = 0;
		ctr = 0;
		break;

	    case SYNC:				/* flush pipe */
		while (readmon(&c, 1) > 0) {
		}
		ctr = 0;
		break;

	    default:
		syserr("bad arg '%d' in get_scan", mode);
	}

#ifdef	xPTR3
	tTfp(54, 1, " ctr %d: '%c' (0%o).\n", ctr & I1MASK, c, c);
#endif

	return (ctr ? c : 0);
}
