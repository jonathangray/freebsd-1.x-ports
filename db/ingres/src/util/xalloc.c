#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <useful.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)xalloc.c	8.1	12/31/84)

/*
**  XALLOC -- allocate block of memory.
**
**	This is just like malloc, except that it is guaranteed
**	to succeed.  It will syserr if it fails.
**
**	Parameters:
**		sz -- size in bytes of memory area to allocate.
**
**	Returns:
**		pointer to area allocated.
**
**	Side Effects:
**		none.
**
**	Trace Flags:
**		none.
*/

/* it's birthday time again */
#define MAGIC_NUMBER	0x280459ac;

static long	magic = MAGIC_NUMBER;

void *
xalloc(int sz, char zeroit, char errquit)
{
	register void	*p;

	if ((p = (void *) malloc(sz + sizeof(magic))) == (void *) NULL) {
		if (errquit) {
			syserr("Out of memory");
		}
		return((void *) NULL);
	}
	(void) memcpy(p++, &magic, sizeof(magic));
	if (zeroit) {
		memset(p, 0, sz);
	}
	return(p);
}

void
xfree(void *p)
{
	if (p != (void *) NULL && memcmp(--p, &magic, sizeof(magic)) == 0) {
		free(p);
	}
}

void *
xrealloc(void *p, int siz)
{
	void	*was;

	was = --p;
	if ((p = realloc(p, siz)) == (void *) NULL) {
		return(p);
	}
	if (was != p) {
		(void) memcpy(p, &magic, sizeof(magic));
	}
	return(p + 1);
}
