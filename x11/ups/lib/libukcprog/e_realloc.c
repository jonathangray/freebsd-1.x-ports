/*  e_realloc() -- Error checking realloc. */

char ukcprog_realloc_sccsid[] = "@(#)e_realloc.c	1.6 26/4/92 UKC";

#ifndef __STDC__
#include <sys/types.h>	/* for size_t */
#endif

#include <stdio.h>	/* for NULL */
#include <ukcstdlib.h>

#include "ukcprog.h"


voidptr
e_realloc(old, size)
voidptr old;
size_t size;
{
	char *new;

	if (old == NULL)
		return e_malloc(size);

	if (size == 0) {
		free(old);
		return NULL;
	}
		
	if ((new = realloc(old, (size_t)size)) == NULL)
		panic("realloc failed in e_realloc");

	return new;
}
