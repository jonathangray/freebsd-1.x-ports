#include <useful.h>
#include "sccs.h"

SCCSID(@(#)scompare.c	8.3	1/17/85)

/*
**  STRING COMPARE
**
**	The strings 'a_ptr' and 'b_ptr' are compared.  Blanks are
**	ignored.  The first string may be no longer than 'a_len'
**	bytes, and the second string may be no longer than 'b_len'
**	bytes.  If either length is zero, it is taken to be very
**	long.  A null byte also terminates the scan.
**
**	Compares are based on the ascii ordering.
**
**	Shorter strings are less than longer strings.
**
**	Return value is positive one for a > b, minus one for a < b,
**	and zero for a == b.
**
**	Examples:
**		"abc" > "ab"
**		"  a bc  " == "ab  c"
**		"abc" < "abd"
*/
int
scompare(void *a_ptr, int a_len, void *b_ptr, int b_len)
{
	register char	a;
	register int	al;
	register int	bl;
	char		*ap;
	char		*bp;
	char		b;

	ap = (char *) a_ptr;
	bp = (char *) b_ptr;
	al = a_len;
	if (al == 0)
		al = MAXI4;
	bl = b_len;
	if (bl == 0) {
		bl = MAXI4;
	}

	for (;;) {
		/* supress blanks in both strings */
		while ((a = *ap) == ' ' && al > 0) {
			al--;
			ap++;
		}
		if (al == 0) {
			a = 0;
		}
		while (*bp == ' ' && bl > 0) {
			bl--;
			bp++;
		}
		b = (bl == 0) ? 0 : *bp;

		/* do inequality tests */
		if (a < b) {
			return (-1);
		}
		if (a > b) {
			return (1);
		}
		if (a == 0) {
			return (0);
		}

		/* go on to the next character */
		ap++;
		al--;
		bp++;
		bl--;
	}
}
