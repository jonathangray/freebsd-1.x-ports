/*
 * strrchr - find last occurrence of a character in a string
 */

#define	NULL	0

char *				/* found char, or NULL if none */
strrchr(s, charwanted)
 char *s;
register char charwanted;
{
	register  char *scan;
	register  char *place;

	place = NULL;
	for (scan = s; *scan != '\0'; scan++)
		if (*scan == charwanted)
			place = scan;
	if (charwanted == '\0')
		return(scan);
	return(place);
}
