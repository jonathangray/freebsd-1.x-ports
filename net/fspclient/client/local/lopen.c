/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND open none "specify the remote host (and optionally port/directory)"
COMMAND host none "same as `open'"
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
    char *newhost;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT,"(to) ");
	    newhost = readword();
	}
	else
	    newhost = 0;
    }
    else
	newhost = argv[1];

    if (newhost == 0 || *newhost == '\0')
	return 1;

    if (env_host)
	(void)free(env_host);
    env_host = strdup(newhost);

    if (argc > 2)
    {
       if (env_port)
           (void)free(env_port);
	env_port = strdup(argv[2]);
    }
    else
    {
       if (env_port)
	   (void)free(env_port);
	env_port = strdup("21");
    }

    if (env_dir)
       (void)free(env_dir);
    env_dir = strdup((argc > 3)? argv[3]: "/");

    disconnect();

    return 0;
}
