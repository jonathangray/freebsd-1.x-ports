/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND burst none "set the number of quick retries made"
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
    char *newburst;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT, "(count) ");
	    newburst = readword();
	}
	else
	    newburst = (char*) 0;
    }
    else
	newburst = argv[1];

    if (newburst == (char*) 0)
	return 1;

    burst_max = atoi(newburst);
    if (burst_max < 1)
	burst_max = 1;

    ffprintf(STDINFO, "burst count set to %d\n", burst_max);

    return 0;
}
