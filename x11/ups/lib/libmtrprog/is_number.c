/* is_number.c - return TRUE is a string is a decimal number */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char utils_is_number_sccsid[] = "@(#)is_number.c	1.4 26/4/92 (UKC)";

#include <ctype.h>
#include <local/ukcprog.h>

#include "utils.h"

int
is_number(s)
const char *s;
{
	for (; *s != '\0'; s++)
		if (!isdigit(*s))
			return FALSE;
	return TRUE;
}
