/*
 * emalloc - malloc with error() called when out of space
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "libc.h"

extern void error();

char *
emalloc(amount)
unsigned amount;
{
	register char *it;
	char camount[25];		/* Enough to sprintf an unsigned. */

	it = malloc(amount);
	if (it == NULL) {
		sprintf(camount, "%u", amount);
		error("malloc(%s) failed", camount);
	}	

	return(it);
}
