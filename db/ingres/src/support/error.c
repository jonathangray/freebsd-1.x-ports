#include <stdio.h>
#include <stdarg.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#define INGRES_IUTIL
#define INGRES_GUTIL
#include "protos.h"

SCCSID(@(#)error.c	8.1	12/31/84)

#define	ERRDELIM	'~'

/*
**  PROCESS ERROR MESSAGE (Standalone override)
**
**	This routine replaces the "error" routine for use in
**	standalone routines such as creatdb and printr.  Its usage
**	is identical to that of normal "error".
**
**	This routine is assumed to be called by process five; hence,
**	all error messages it produces have a 5000 offset.
**
*/
int
error(int number, ...)
{
	FILE		*iop;
	int		i;
	register char	*p;
	char		*fmt;
	register int	err;
	char		buf[10];
	register char	c;
	va_list		vp;

	va_start(vp, number);
	fmt = va_arg(vp, char *);
	va_end(vp);
	err = number;
	if ((iop = fopen(errfilen(5), "r")) == NULL)
		syserr("error: open");

	/* read in the code and check for correct */
	for (;;) {
		p = buf;
		while ((c = getc(iop)) != '\t') {
			if (c <= 0) {
				/* no code exists, print the first parm */
				printf("%d: %s\n\n", err, fmt);
				fclose(iop);
				return (err);
			}
			*p++ = c;
		}
		*p = 0;
		i = atoi(buf);
		if (i != err) {
			while ((c = getc(iop)) != ERRDELIM)
				if (c <= 0)
					syserr("proc_error: format err %d", err);
			getc(iop);	/* throw out the newline */
			continue;
		}

		/* got the correct line, print it doing param substitution */
		printf("%d: ", err);
		c = '\n';
		for (;;) {
			c = getc(iop);
			if (c == EOF || c == ERRDELIM) {
				printf("\n");
				fclose(iop);
				return(err);
			}
			if (c == '%') {
				c = getc(iop);
				va_start(vp, number);
				for (i = '0' ; i <= c ; i++) {
					p = va_arg(vp, char *);
				}
				va_end(vp);
				for ( ; (c = *p) != 0 ; p++) {
					putchar(c);
				}
				continue;
			}
			putchar(c);
		}
	}
}

void
nferror(int number, ...)
{
	va_list	vp;

	va_start(vp, number);
	error(number, vp);
	va_end(vp);
}
