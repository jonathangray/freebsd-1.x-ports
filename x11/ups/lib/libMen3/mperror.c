/* mperror.c - Mperror code */

/*  Copyright 1991 John Bovey, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char Men3_mperror_c_sccsid[] = "@(#)mperror.c	1.6 4/7/91 (UKC)";

#include <stdio.h>

#include <local/wn.h>
#include "menu3.h"
#include "menu_priv.h"

static const char * merrmes[] = {
	"no error",
	"menu table full",
	"menu descriptor not of an open menu",
	"menu descriptor out of range",
	"can't read menu from menu file",
	"can't open menu file",
	"bad window file descriptor",
	"menu not displayed",
	"bad menu function argument",
	"message area error",
	"menerr out of range",
	NULL
};

/*  print out a menu error message
 */
int
Mperror(str)
const char * str;
{
	register int i;

	/* protect against out of range error number.
	 */
	menerr = menerr < 0 ? 1000 : menerr;
	for (i = 0; i <= menerr; i++)
		if (merrmes[i] == NULL)
			break;
	fprintf(stderr,"%s: %s\7\n",str,merrmes[i - 1]);
	return(0);
}
