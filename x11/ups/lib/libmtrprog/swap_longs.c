/* swap_longs.c - byte swap an array of longs */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_swap_longs_sccsid[] = "@(#)swap_longs.c	1.4 26/4/92 (UKC)";

#include <local/ukcprog.h>

#include "utils.h"

void
swap_longs(base, count)
long *base;
int count;
{
	unsigned char *s, *lim;

	lim = (unsigned char *)(base + count);
	for (s = (unsigned char *)base; s < lim; s += sizeof(int)) {
		unsigned char c;

		c = s[0];
		s[0] = s[3];
		s[3] = c;

		c = s[1];
		s[1] = s[2];
		s[2] = c;
	}
}
