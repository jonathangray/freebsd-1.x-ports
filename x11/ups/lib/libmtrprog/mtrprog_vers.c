/* mtrprog_vers.c - version number routine for the util library */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char mtrprog_vers_sccsid[] = "@(#)mtrprog_vers.c	1.5 26/4/92 (UKC)";

#include <local/ukcprog.h>

#include "utils.h"
#include "sccsdata.h"

/*  Return the version number of the arg library.
 */
const char *
mtrprog_version()
{
	return _mtrprog_sccsdata[0];
}
