/*
 * active file handling routines
 *
 * format of file:
 *	<groupname> ' ' <#> ' ' <#> ' ' flag '\n'
 *			(seq)	(low)
 */

#include "defs.h"

static int lineno;
static active	*alist;

static
getline(f, g, d, d2)
register FILE *f;
char *g, *d, *d2;
{
	register int c;
	register char *s;

	lineno++;
	s = g;
	while ((c = getc(f)) != ' ' && c != EOF)
		*s++ = c;
	*s = '\0';

	if (c != EOF) {
		s = d;
		while ((c = getc(f)) != EOF && isdigit(c))
			*s++ = c;
		*s = '\0';

		s = d2;
		if (c == ' ')
			while ((c = getc(f)) != EOF && isdigit(c))
				*s++ = c;
		*s = '\0';

		if (c == ' ')
			while ((c = getc(f)) != EOF && c != '\n')
				;		/* eat flag */
	}

	if (c != EOF && (c != '\n' || !*d || !*d2))
		error("%s: bad format: line %d", "active", lineno);
	return c != EOF;
}


/*
 * build internal active file structure
 */
active *
readactive()
{
	register FILE	*f;
	register active	*ap, *last;
	char gbuf[BUFSIZ / 2], dbuf[BUFSIZ / 4], dbuf2[BUFSIZ / 4];

	alist = last = NULL;
	f = fopenf(ctlfile("active"), "r");
	lineno = 0;
	while (getline(f, gbuf, dbuf, dbuf2)) {
		ap = NEW(active);
		ap->a_name = newstr(gbuf);
		ap->a_seq = atol(dbuf);
		ap->a_low = atol(dbuf2);
		ap->a_next = NULL;
		if (!alist)
			alist = ap;
		else
			last->a_next = ap;
		last = ap;
	}
	fclose(f);
	return alist;
}


/*
 * return pointer to named group
 */
active *
activep(grp)
register char *grp;
{
	register active	*ap;

	for (ap = alist; ap; ap = ap->a_next)
		if (CMP(grp, ap->a_name) == 0)
			break;
	return ap;
}
