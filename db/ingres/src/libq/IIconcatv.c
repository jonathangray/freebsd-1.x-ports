#include <stdio.h>
#include <stdarg.h>

#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIconcatv.c	8.1	12/31/84)


/*
**  IIconcatv -- concatenate strings into a buffer.
**
**	Parameters:
**		buf -- result buffer
**		args -- first of a 0 terminated series of string pointers.
**	
**	Returns:
**		a pointer to the null byte appended.
**
*/
char *
IIconcatv(char *buf, ...)
{
	register char	*p;
	register char	*s;
	va_list		vp;

	va_start(vp, buf);
	p = &buf[strlen(buf)];
	while ((s = va_arg(vp, char *)) != (char *) NULL) {
		/* move the current arg over */
		while ((*p = *s++) != 0)
			p++;
	}
	*p = 0;
	va_end(vp);
	return(p);
}
