/*
 * Emulation of misc. ANSI C stdio functions
 */

/* stdio.c,v 1.1.1.1 1993/05/21 05:37:51 cgd Exp */

#include <stdio.h>

#ifndef __STDC__
#define const
#define volatile
#endif

#if 1
int
remove(name)
	const char *name;
{
	return unlink(name);
}
#endif

#if _V7
int
rename(oname, nname)
	const char *oname, *nname;
{
	return link(oname, nname) == 0 && unlink(oname) == 0 ? 0 : -1;
}
#endif

