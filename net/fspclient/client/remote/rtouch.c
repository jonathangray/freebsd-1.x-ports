/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND touch local "cause zero length files to be created on the server"
 *  ---INFOEND---  */

#include "client.h"

static FILE *devnull;
static int dirty;

static int
#ifndef ANSI_PROTOTYPES
do_touch(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_touch(char *name)
#endif /* ANSI_PROTOTYPES */
{
    struct stat remote;

    if (!validate_operation(name, UTIL_UPLOAD))
	return -1;

    if (util_stat(name, &remote) < 0)
    {
	if (util_upload(name, devnull) == 0)
	    dirty = 1;
	else
	    return -1;
    }

    return 0;
}

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
    int retval;

    if (argc > 1)
    {
	int old_client_trace = client_trace;

	devnull = fopen("/dev/null", "r");
	if (devnull == 0)
	{
	    ffprintf(STDERR, "touch: can't open `/dev/null'!\n");
	    return 1;
	}

	client_trace = 0;
	dirty  = 0;

	retval = util_process_arglist(argv + 1, do_touch);

	if (dirty)
	    util_dirtydir(".");
	client_trace = old_client_trace;

	(void)fclose(devnull);
    }
    else
	retval = 0;

    return retval;
}
