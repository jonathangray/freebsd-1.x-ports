/* fpgetline.c - read an arbitrary length line from a stdio stream */

char ukcprog_fpgetline_sccsid[] = "@(#)fpgetline.c	1.4 26/4/92 UKC";

#ifndef __STDC__
#include <sys/types.h>		/* for size_t */
#endif

#include <stdio.h>
#include <ukcstdlib.h>

#include "ukcprog.h"

#define SMALLBUF_SIZE	200

char *
fpgetline(fp)
FILE *fp;
{
	static char *buf;
	static size_t bufsize = 0;
	int ch, pos;

	if (bufsize == 0) {
		bufsize = 80;
		buf = e_malloc(bufsize + 1);
	}

	for (pos = 0; (ch = getc(fp)) != EOF && ch != '\n'; buf[pos++] = ch) {
		if (pos == bufsize) {
			bufsize *= 2;
			buf = e_realloc(buf, bufsize + 1);
		}
	}
	buf[pos] = '\0';

	/*  If we have a huge buffer from the last call and now have a
	 *  short line, try to dump the excess.
	 */
	if (pos <= SMALLBUF_SIZE && bufsize > 5000) {
		char *smallbuf;
		
		if ((smallbuf = realloc(buf, SMALLBUF_SIZE + 1)) != NULL) {
			buf = smallbuf;
			bufsize = SMALLBUF_SIZE;
		}
	}

	return (pos == 0 && ch == EOF) ? NULL : buf;
}
