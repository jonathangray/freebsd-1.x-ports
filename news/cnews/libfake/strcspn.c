/*
 * strcspn - find length of initial segment of s consisting entirely
 * of characters not from reject
 */

int
strcspn(s, reject)
 char *s;
 char *reject;
{
	register  char *scan;
	register  char *rscan;
	register int count;

	count = 0;
	for (scan = s; *scan != '\0'; scan++) {
		for (rscan = reject; *rscan != '\0';)	/* ++ moved down. */
			if (*scan == *rscan++)
				return(count);
		count++;
	}
	return(count);
}
