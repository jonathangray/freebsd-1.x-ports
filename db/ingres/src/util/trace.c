#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "useful.h"
#include "sccs.h"

SCCSID(@(#)trace.c	8.1	12/31/84)

#define	tTNCUTOFF	30

short		*tT;
int		tTsize;
char		*tTp, tTsav, tTany;
void		(*tTsetf)(int fun, int pt);

void
tTon(register int fun, register int pt)
{
	if (fun >= tTsize || fun < 0)
		return;

	if (pt >= 0)
		tT[fun] |= (1<<pt%16);
	else
		tT[fun] = 0177777;
}

void
tToff(register int fun, register int pt)
{
	if (fun >= tTsize || fun < 0)
		return;

	if (pt >= 0)
		tT[fun] ^= (1<<pt%16);
	else
		tT[fun] = 0;
}

int
tTnext(void)
{
	register char	*c;
	int	ix;

	c = tTp;
	while (isdigit(*tTp)) {
		tTp++;
	}
	tTsav = *tTp;
	*tTp = '\0';
	ix = atoi(c);
	*tTp++ = tTsav;
	return (ix);
}

void
tTurn(register int fno)
{
	register int	pt;

	if (tTsav == '.') {
		while (tTsav == '.') {
			pt = tTnext();
			(*tTsetf)(fno, pt);
		}
	} else
		(*tTsetf)(fno, -1);
}

void
tTflag(char *args)
{
	register int	fno;
	int		f;

	tTany++;
	tTp = args;
	if (*tTp == '\0') {
		for (fno = tTNCUTOFF; fno < tTsize; fno++)
			(*tTsetf)(fno, -1);
		return;
	}
	do {
		fno = tTnext();
		tTurn(fno);

		if (tTsav == '/') {
			f = fno + 1;
			fno = tTnext();
			while (f < fno)
				(*tTsetf)(f++, -1);
			tTurn(fno);
		}
	}  while(tTsav != '\0');
}

int
tTrace(char **argv, char tflag, short *tvect, int tsize)
{
	register char	**pp;
	register bool	rtval;

	tT = tvect;
	tTsize = tsize;
	tTsetf = tTon;
	rtval = FALSE;

	for (pp = argv; *pp != NULL; pp++) {
		if ((*pp)[0] != '-' || (*pp)[1] != tflag)
			continue;
		tTflag(&(*pp)[2]);
		rtval = TRUE;
	}
	return (rtval);
}

/*
**  TTAMPER -- internal interface to set & clear trace flags
**
**	This routine is called from the ctlmod or whatever.
**
**	Parameters:
**		line -- a line like the normal trace flag lines.
**		tflag -- the trace flag to deal with.
**		tvect -- a pointer to a trace vector.
**		tsize -- the size of the trace vector.
**
**	Returns:
**		??
**
**	Side Effects:
**		none.
*/
int
tTamper(char *line, char tflag, short *tvect, int tsize)
{
	register char	*p;
	register char	*endp;
	register bool	rtval;
	char		save;
	short		*otvect;
	int		otsize;

	otvect = tT;
	otsize = tTsize;
	tT = tvect;
	tTsize = tsize;
	rtval = FALSE;

	for (p = line; *p != '\n' && *p != '\0'; p++) {
		switch (*p) {
		  case '+':
			tTsetf = tTon;
			break;

		  case '-':
			tTsetf = tToff;
			break;

		  default:
			continue;
		}
		if (*(++p) != tflag) {
			p--;
			continue;
		}

		for (endp = ++p; *endp != ' ' && *endp != '\t' && *endp != '\0' && *endp != '\n'; endp++)
			continue;

		save = *endp;
		*endp = '\0';

		tTflag(p);

		*endp = save;
		p = --endp;
		rtval = TRUE;
	}
	tT = otvect;
	tTsize = otsize;
	return (rtval);
}

/*
**  CHECK TRACE FLAG AND PRINT INFORMATION
**
**	This routine is equivalent to
**		if (tTf(m, n))
**			printf(a1, a2, a3, a4, a5, a6);
**
**	and can be called to reduce process space.  The return value
**	is the value of the flag.
*/

#define	tTf(a, b)	((b < 0) ? tT[a] : (tT[a] & (1 << b)))

int
tTfp(int m, int n, char *fmt, ...)
{
	register int	rtval;
	va_list		vp;

	va_start(vp, fmt);
	if ((rtval = tTf(m, n)) != 0) {
		vfprintf(stdout, fmt, vp);
	}
	va_end(vp);
	return (rtval);
}
