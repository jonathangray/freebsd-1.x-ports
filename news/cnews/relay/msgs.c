/*
 * print common messages
 */

#include <stdio.h>
#include <sys/types.h>
#include "news.h"
#include "headers.h"
#include "article.h"
#include "msgs.h"
#include "rmsgs.h"

void
fulldisk(art, file)			/* complain once & set status bits */
register struct article *art;
char *file;
{
	if (!(art->a_status&ST_DISKFULL))
		art->a_status |= prfulldisk(file);
}
