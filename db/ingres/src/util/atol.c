#include <useful.h>
#include "sccs.h"

SCCSID(@(#)atol.c	8.2	1/16/85)

/*
**  ASCII CHARACTER STRING TO 32-BIT INTEGER CONVERSION
**
**	`a' is a pointer to the character string, `i' is a
**	pointer to the doubleword which is to contain the result.
**
**	The return value of the function is:
**		zero:	succesful conversion; `i' contains the integer
**		+1:	numeric overflow; `i' is unchanged
**		-1:	syntax error; `i' is unchanged
**
**	A valid string is of the form:
**		<space>* [+-] <space>* <digit>* <space>*
*/
int
ingres_atol(register char *a, long *i)
{
	register int	sign;	/* flag to indicate the sign */
	long		x;	/* holds the integer being formed */
	register char	c;

	sign = 0;
	/* skip leading blanks */
	while (*a == ' ')
		a++;
	/* check for sign */
	switch (*a) {

	  case '-':
		sign = -1;

	  case '+':
		while (*++a == ' ');
	}

	/* at this point everything had better be numeric */
	x = 0;
	while ((c = *a) <= '9' && c >= '0') {
		/* check for overflow */
		/* 2 ** 31 = 2147483648 */
		if (x > MAXI8 )
			return (1);
		x = x * 10 + (c - '0');
		if (x < 0)	/* check if new digit caused overflow */
			return (1);
		a++;
	}

	/* eaten all the numerics; better be all blanks */
	while ((c = *a++) != 0) {
		if (c != ' ')			/* syntax error */
			return (-1);
	}
	*i = sign ? -x : x;
	return (0);		/* successful termination */
}
