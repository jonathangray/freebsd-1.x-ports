/*
 * strpbrk - find first occurrence of any char from breakat in s
 */

#define	NULL	0

char *				/* found char, or NULL if none */
strpbrk(s, breakat)
 char *s;
 char *breakat;
{
	register  char *sscan;
	register  char *bscan;

	for (sscan = s; *sscan != '\0'; sscan++) {
		for (bscan = breakat; *bscan != '\0';)	/* ++ moved down. */
			if (*sscan == *bscan++)
				return(sscan);
	}
	return(NULL);
}
