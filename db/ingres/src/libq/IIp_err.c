#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <pipes.h>
#include <ingres.h>
#include <aux.h>
#include <version.h>
#include "IIglobals.h"
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIp_err.c	8.1	12/31/84)


/*
**  IIp_err 
**
**	This routine processes an IIerror call for printout.This involves doing
**	a lookup in the .../files/error? files, where ? is the thous-
**	ands digit of the error number.  The associated error message
**	then goes through parameter substitution and is printed.
**
**	In the current version, the error message is just printed.
*/
int
IIp_err(int err_num, int argc, char **argv)
{
	register char	*p;
	char		buf[512];
	int		i;
	iob_t	iob;
	register char	c;

	/* IIfopen the appropriate error file */
	p = buf;
	IIerrfilen(err_num / 1000, p);

#ifdef xETR2
	if (IIdebug > 1)
		printf("IIp_err : with IIerrfilen ret \"%s\"\n", p);
#endif

	if (IIfopen(p, &iob) < 0)
		return (0);

	/* read in the code and check for correct */
	for (;;) {
		p = buf;
		while ((c = IIgetc(&iob)) != '\t') {
			if (c <= 0) {
				if (IIclose(&iob) < 0)
					IIsyserr("IIp_err: bad close 1");
				return (0);
			}
			*p++ = c;
		}
		*p = 0;
		if (IIatoi(buf, &i))
			IIsyserr("IIp_err: bad error file %d\n%s",
				err_num, buf);
		if (i != err_num) {
			while ((c = IIgetc(&iob)) != ERRDELIM)
				if (c <= 0)
					IIsyserr("IIp_err: format err %d", err_num);
			IIgetc(&iob);	/* throw out the newline */
			continue;
		}

		/* got the correct line, print it doing parameter substitution */
		printf("%d: ", err_num);
		for (;;) {
			c = IIgetc(&iob);
			if (c <= 0 || c == ERRDELIM) {
				printf("\n");
				 if (IIclose(&iob) < 0)
					IIsyserr("IIp_err: bad close 2");
				return (1);
			}
			if (c == '%') {
				c = IIgetc(&iob);
				for (p = argv[c - '0']; (c = *p) != 0 ; p++) {
					if (c < 040 || c >= 0177)
						printf("\\%o", c);
					else
						putchar(c);
				}
				continue;
			}
			putchar(c);
		}
	}
}

/*
** IIerrfilen -- Returns the pathname where the error file can be found
**	into "buf".
**
**	It is assumed that the error number cannot be more than 999.
**	Returned is concat of : IIPathname, "/files/error", MAJOR_VERSION (w/o mod),
**	"_num".
*/

char *
IIerrfilen(int num, char *buf)
{
	register char	*cp;
	char		ver[12];

	IIconcatv(ver, MAJOR_VERSION, 0);

	/* now insert the "_X" */
	cp = &ver[strlen(ver)];
	*cp++ = '_';
	*cp = '\0';
	IIconcatv(ver, ver, IIitos(num), 0);


	return (IIconcatv(buf, IIPathname, "/files/error", ver, 0));
}
