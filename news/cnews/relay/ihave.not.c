/*
 * Reject the Usenet ihave/sendme control messages. (NCMP)
 */

#include <stdio.h>
#include <sys/types.h>

#include "news.h"
#include "headers.h"
#include "article.h"

static void
ignore(cmd, args)
char *cmd, *args;
{
	(void) fprintf(stderr, "%s: `%s %s' control ignored\n", progname, cmd, args);
}

/* ARGSUSED art */
void
ihave(args, art)
char *args;
struct article *art;
{
	ignore("ihave", args);
}

/* ARGSUSED art */
void
sendme(args, art)
char *args;
struct article *art;
{
	ignore("sendme", args);
}
