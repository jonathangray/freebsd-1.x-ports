/*
 * gethdr - read an entire RFC 822 or 1036 header "line",
 *	including continuations
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <fgetmfs.h>
#include <sys/types.h>
#include "news.h"
#include "libc.h"

static int rfc = 1036;

sethdrrfc(newrfc)
int newrfc;
{
	rfc = newrfc;
}

/*
 * Read the first line; if it's a header, repeatedly read lines until a
 * non-continuation line is found.  For each continuation line, grow
 * hdr to accomodate it and append it to hdr.
 * *limitp is updated by subtracting the number of bytes read.
 */
char *					/* malloced; caller must not free */
gethdr(in, limitp, ishdrp)
FILE *in;
register long *limitp;
int *ishdrp;
{
	register int c, hdrlen, contlen, limitset = *limitp >= 0;
	register char *contin = NULL;
	static char *hdr = NULL;

	nnfree(&hdr);
	*ishdrp = NO;
	hdr = fgetmfs(in, intlimit(*limitp), CONT_NO);
	if (hdr == NULL)
		return hdr;
	hdrlen = strlen(hdr);
	*limitp -= hdrlen;
	*ishdrp = ishdr(hdr);
	if (!*ishdrp)
		return hdr;
	while (hdr != NULL && (!limitset || *limitp > 1) &&
	    (c = getc(in)) != EOF) {
		(void) ungetc(c, in);

		if (!iswhite(c))
			break;
		contin = fgetmfs(in, intlimit(*limitp), CONT_NO);
		if (contin == NULL)
			break;

		contlen = strlen(contin);
		*limitp -= contlen;
		hdr = realloc(hdr, (unsigned)(hdrlen + contlen + SIZENUL));
		if (hdr != NULL) {
			(void) strcpy(hdr + hdrlen, contin);
			hdrlen += contlen;
		}
		free(contin);
		contin = NULL;
	}
	return hdr;
}

/*
 * Is s an RFC 822 (or 1036) header line?
 * If a colon is seen before whitespace, it is.
 */
int
ishdr(s)
register char *s;
{
	register char *cp = s;
	register int c;

	while ((c = *cp) != '\0' && !(isascii(c) && isspace(c)) && c != ':')
		++cp;
	return c == ':' && cp > s && (rfc != 1036 || cp[1] == ' ');
}

/*
 * make longlimit fit in an int.  for now, restrict it to 30000 if it won't fit.
 */
STATIC int
intlimit(longlimit)
register long longlimit;
{
	register int limit = (int)longlimit;	/* hmm, could overflow */

	if (limit != longlimit)		/* longlimit won't fit in an int? */
		limit = 30000;			/* HACK */
	return limit;
}
