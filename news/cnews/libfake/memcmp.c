/*
 * memcmp - compare bytes
 */

int				/* <0, == 0, >0 */
memcmp(s1, s2, size)
 char * s1;
 char * s2;
int size;
{
	register  char *scan1;
	register  char *scan2;
	register int n;

	scan1 = s1;
	scan2 = s2;
	for (n = size; n > 0; n--)
		if (*scan1 == *scan2) {
			scan1++;
			scan2++;
		} else
			return(*scan1 - *scan2);

	return(0);
}
