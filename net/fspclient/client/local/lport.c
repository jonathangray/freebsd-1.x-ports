/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND port none "set the connection port to the remote system"
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
    char *newport;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT,"(port) ");
	    newport = readword();
	}
	else
	    newport = 0;
    }
    else
	newport = argv[1];

    if (newport == 0)
	return 1;

    if (env_port)
        (void)free(env_port);
    env_port = strdup(newport);

    disconnect();

    return 0;
}
