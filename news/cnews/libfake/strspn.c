/*
 * strspn - find length of initial segment of s consisting entirely
 * of characters from accept
 */

int
strspn(s, accept)
 char *s;
 char *accept;
{
	register  char *sscan;
	register  char *ascan;
	register int count;

	count = 0;
	for (sscan = s; *sscan != '\0'; sscan++) {
		for (ascan = accept; *ascan != '\0'; ascan++)
			if (*sscan == *ascan)
				break;
		if (*ascan == '\0')
			return(count);
		count++;
	}
	return(count);
}
