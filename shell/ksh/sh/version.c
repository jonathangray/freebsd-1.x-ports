/*
 * value of $KSH_VERSION
 */

#ifndef lint
static char *RCSid = "$Id: version.c,v 1.1 1994/04/16 21:38:49 sean Exp $";
#endif

#include "stdh.h"
#include <setjmp.h>
#include "sh.h"
#include "patchlevel.h"

char ksh_version [] =
	"KSH_VERSION=@(#)PD KSH v4.9 93/09/29";


