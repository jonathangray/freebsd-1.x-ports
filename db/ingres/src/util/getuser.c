#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <aux.h>
#include "sccs.h"

#include <protos.h>

SCCSID(@(#)getuser.c	8.1	12/31/84)

/*
**  GET LINE FROM USER FILE
**
**	Given a user code (a two byte string), this routine returns
**	the line from .../files/users into `buf'.  The users
**	file is automatically opened, and it is closed if getperm
**	is called with `code' == 0.
**
**	If `code' == -1 then getuser will reinitialize itself.
**	This will guarantee that getuser will reopen the file
**	if (for example) an interrupt occured during the previous
**	call.
*/

int
getuser(char *code, char *buf)
{
	static FILE	*userf;
	register char	c;
	register char	*bp;
	
	if (code == 0) {
		if (userf != NULL)
			fclose(userf);
		userf = NULL;
		return (0);
	}
	if (code == (char *) -1) {
		userf = NULL;
		return (0);
	}
	if (userf == NULL) {
		userf = fopen(ztack(Pathname, "/files/users"), "r");
		if (userf == NULL)
			syserr("getuser: open err");
	}
	rewind(userf);
	
	for (;;) {
		bp = buf;
		if (fgets(bp, MAX_LINE_SIZE, userf) == NULL)
			return (1);
		*strchr(bp, '\n') = '\0';
		while ((c = *bp++) != ':')
			if (c == '\0')
				return (1);
		if (bequal(bp, code, 2))
			return (0);
	}
}
