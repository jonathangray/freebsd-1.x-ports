/* autoinit.c 
	vi:se ts=3 sw=3:
 */

/* $Id: autoinit.c,v 1.1 1994/02/19 16:03:07 ache Exp $ 
 * $Log: autoinit.c,v $
 * Revision 1.1  1994/02/19 16:03:07  ache
 * Initial revision
 *
 * Revision 4.1  1994/01/12  16:10:20  espie
 * Fixed up last minute problems.
 *
 * Revision 4.0  1994/01/11  17:41:28  espie
 * *** empty log message ***
 *
 * Revision 1.7  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.6  1994/01/07  15:06:26  Espie
 * Id
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include "defs.h"
#include "extern.h"

ID("$Id: autoinit.c,v 1.1 1994/02/19 16:03:07 ache Exp $")

LOCAL struct clist
	{
	struct clist *next;
	void (*func) P((void));
	} *list = 0;
	

void at_end(cleanup)
void (*cleanup) P((void));
	{
#ifdef USE_AT_EXIT
	atexit(cleanup);
#else
	struct clist *new;
	new = (struct clist *)malloc(sizeof(struct clist));
	if (!new)
		{
		(*cleanup)();
		end_all("Allocation problem");
		}
	new->next = list;
	new->func = cleanup;
	list = new;
#endif
	}
	
void end_all(s)
char *s;
	{
#ifndef USE_AT_EXIT
	struct clist *p;
#endif
	if (s)
		notice(s);
#ifndef USE_AT_EXIT
	for (p = list; p; p = p->next)
		(p->func)();			/* don't bother freeing (malloc) */
#endif
	exit(s ? 10 : 0);
	}
