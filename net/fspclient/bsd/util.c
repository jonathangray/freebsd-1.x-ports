/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)util.c	5.7 (Berkeley) 4/8/90";
#endif /* not lint */

#include "client.h"
#include "ls.h"
#include <ctype.h>

void
#ifndef ANSI_PROTOTYPES
prcopy(src, dest, len)
	register char *src, *dest;
	register int len;
#else /* ANSI_PROTOTYPES */
prcopy(register char *src, register char *dest, register int len)
#endif /* ANSI_PROTOTYPES */
{
	register int ch;

	while(len--) {
		ch = *src++;
		*dest++ = isprint(ch) ? ch : '?';
	}
}

char
#ifndef ANSI_PROTOTYPES
*emalloc(size)
	unsigned int size;
#else /* ANSI_PROTOTYPES */
*emalloc(unsigned int size)
#endif /* ANSI_PROTOTYPES */
{
	char *retval;

	if (!(retval = (char *)malloc(size)))
		nomem();
	return(retval);
}

void
#ifndef ANSI_PROTOTYPES
nomem()
#else /* ANSI_PROTOTYPES */
nomem(void)
#endif /* ANSI_PROTOTYPES */
{
	ffprintf(STDERR, "ls: out of memory.\n");
	ls_bad(1);
}

void
#ifndef ANSI_PROTOTYPES
usage()
#else /* ANSI_PROTOTYPES */
usage(void)
#endif /* ANSI_PROTOTYPES */
{
	ffprintf(STDERR, "usage: ls [-1ACFLRacdfgiklqrstu] [file ...]\n");
	ls_bad(1);
}
