#include <stdio.h>
#include <stdlib.h>

char *
nemalloc(size)			/* news emalloc - calls errunlock on error */
unsigned size;
{
	register char *result = malloc(size);

	if (result == NULL)
		errunlock("out of memory", "");
	return result;
}
