/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND since local "make ls show only files since time/file given"
 *  ---INFOEND---  */

#include "client.h"
#include "util.h"
#include <ctype.h>

time_t sincetime = 0;

int
#ifndef ANSI_PROTOTYPES
main(argc, argv, envp)
    int argc;
    char *argv[];
    char *envp[];
#else /* ANSI_PROTOTYPES */
main(int argc, char **argv, char **envp)
#endif /* ANSI_PROTOTYPES */
{
    char *newsincetime;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT,"(since) ");
	    newsincetime = readword();
	}
	else
	   newsincetime = 0;
    }
    else
	newsincetime = argv[1];

    if (newsincetime == 0)
	return 1;

    if (isdigit(*newsincetime))
	sincetime = atoi(newsincetime);
    else
    {
	struct stat buf;

	if (stat(newsincetime, &buf) < 0)
	{
	    ffprintf(STDERR, "since: stat(%s): %s\n",
			  newsincetime, sys_errlist[errno]);
	    return 1;
	}
	sincetime = buf.st_mtime;
    }

    return 0;
}
