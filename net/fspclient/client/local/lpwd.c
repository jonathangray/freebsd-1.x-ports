/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND pwd none "print the remote current working directory"
 *  ---INFOEND---  */

#include "client.h"

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
    if (env_host == 0 || env_port == 0)
    {
	ffprintf(STDERR, "pwd: not connected\n");
	return 1;
    }

    ffprintf(STDOUT,"\"%s\" on %s (port %s)\n",
	     env_dir? env_dir: "/", env_host, env_port);

    return 0;
}
