/*
 * dogets - do the gets hack on a counted string: given a string and length
 *	pointer, delete any trailing newline and adjust the length to match.
 */

char *
dogets(s, lenp)
register char *s;
register int *lenp;
{
	register char *nlp = s + *lenp - 1;

	if (*lenp > 0 && *nlp == '\n') {
		*nlp = '\0';			/* stomp innocent newline */
		--*lenp;
	}
	return s;
}
