/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND prompt none "turn prompting on or off"
 *  ---INFOEND---  */

#include "client.h"

int askprompt = 1;

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
    if (argc < 2)
	askprompt = !askprompt;
    else
    {
	if (strcmp(argv[1], "off") == 0)
	    askprompt = 0;
	else if (strcmp(argv[1], "on") == 0)
	    askprompt = 1;
	else if (strcmp(argv[1], "?") != 0)
	    askprompt = (atoi(argv[1]) != 0);
    }

    ffprintf(STDINFO, "prompting is %s\n", askprompt? "on": "off");

    return 0;
}
