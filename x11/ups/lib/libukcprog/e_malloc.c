/* e_malloc() -- error checking malloc */

char ukcprog_malloc_sccsid[] = "@(#)e_malloc.c	1.7 26/4/92 UKC";


#ifndef __STDC__
#include <sys/types.h>	/* for size_t */
#endif

#include <stdio.h>	/* for NULL */
#include <ukcstdlib.h>

#include "ukcprog.h"


voidptr
e_malloc(size)
size_t size;
{
	char *ptr;

	if ((ptr = malloc(size)) == NULL)
		panic("malloc failed in e_malloc");

	return ptr;
}
