#include "sccs.h"
#include <useful.h>

#include "protos.h"

SCCSID(@(#)iocv.c	8.2	2/8/85)

/*
**  INTEGER OUTPUT CONVERSION
**
**	The integer `i' is converted to ascii using itoa and put
**	into the static buffer `buf'.  The address of `buf' is
**	returned.
*/

char *
iocv(int i)
{
	static char	buf[CHAR_SZ];

	itoa(i, buf);
	return (buf);
}
