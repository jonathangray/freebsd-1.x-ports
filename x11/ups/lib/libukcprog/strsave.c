/* strsave.c -- allocate memory and save a copy of a string */

char ukcprog_strsave_sccsid[] = "@(#)strsave.c	1.5 26/4/92 UKC";


#include <ukcstring.h>
#include <ukcstdlib.h>	/* for malloc() */

#include "ukcprog.h"


char *
strsave(s)
const char *s;
{
	char *str;

	str = e_malloc(strlen(s) + 1);

	return strcpy(str, s);
}
