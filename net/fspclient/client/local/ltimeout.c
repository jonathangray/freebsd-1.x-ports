/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND timeout none "set the maximum time (in seconds) to retry for"
 *  ---INFOEND---  */

#include "client.h"
#include "util.h"

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
    char *newtimeout;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT, "(seconds) ");
	    newtimeout = readword();
	}
	else
	    newtimeout = 0;
    }
    else
	newtimeout = argv[1];

    if (newtimeout == 0)
	return 1;

    time_out = atoi(newtimeout);

    ffprintf(STDINFO, "maximum retry time set to %u secs\n", time_out);

    return 0;
}
