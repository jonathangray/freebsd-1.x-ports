/*
 * memcpy - copy bytes
 */

char *
memcpy(dst, src, size)
char * dst;
 char * src;
int size;
{
	register char *d;
	register  char *s;
	register int n;

	if (size <= 0)
		return(dst);

	s = src;
	d = dst;
	if (s <= d && s + (size-1) >= d) {
		/* Overlap, must copy right-to-left. */
		s += size-1;
		d += size-1;
		for (n = size; n > 0; n--)
			*d-- = *s--;
	} else
		for (n = size; n > 0; n--)
			*d++ = *s++;

	return(dst);
}
