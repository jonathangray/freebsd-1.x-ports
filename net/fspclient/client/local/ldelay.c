/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND delay none "set the minimum retry period (in milliseconds)"
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
    char *newdelaytime;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT, "(milliseconds) ");
	    newdelaytime = readword();
	}
	else
	    newdelaytime = (char*) 0;
    }
    else
	newdelaytime = argv[1];

    if (newdelaytime == (char*) 0)
	return 1;

    target_delay = atoi(newdelaytime);
    if (target_delay < MINDELAY)
	target_delay = MINDELAY;

    ffprintf(STDINFO, "minimum retry time set to %d msec\n", target_delay);

    return 0;
}
