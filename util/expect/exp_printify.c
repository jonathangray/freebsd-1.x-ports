/* exp_printify - printable versions of random ASCII strings

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.

*/

#include "expect_cf.h"
#include "tcl.h"
#ifdef NO_STDLIB_H
#include "compat/stdlib.h"
#else
#include <stdlib.h>		/* for malloc */
#endif

/* generate printable versions of random ASCII strings.  Primarily used */
/* by cmdExpect when -d forces it to print strings it is examining. */
char *
exp_printify(s)
char *s;
{
	static int destlen = 0;
	static char *dest = 0;
	char *d;		/* ptr into dest */
	unsigned int need;

	if (s == 0) return("<null>");

	/* worst case is every character takes 3 to printify */
	need = strlen(s)*3 + 1;
	if (need > destlen) {
		if (dest) ckfree(dest);
		dest = ckalloc(need);
		destlen = need;
	}

	for (d = dest;*s;s++) {
		if (*s == '\r') {
			strcpy(d,"\\r");		d += 2;
		} else if (*s == '\n') {
			strcpy(d,"\\n");		d += 2;
		} else if (*s == '\t') {
			strcpy(d,"\\t");		d += 2;
		} else if ((unsigned)*s < 0x20) { /* unsigned strips parity */
			sprintf(d,"\\C%c",*s + '`');	d += 3;
		} else if (*s == 0x7f) {
			/* not syntactically correct, but you get the point */
			strcpy(d,"\\7f");		d += 3;
		} else {
			*d = *s;			d += 1;
		}
	}
	*d = '\0';
	return(dest);
}
