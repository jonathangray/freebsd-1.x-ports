/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND shell none "run a subcommand"
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
    int cpid, status = 0;
#ifndef ANSI_PROTOTYPES
    RETSIGTYPE (*old_int)(), (*old_quit)();
#else /* ANSI_PROTOTYPES */
    RETSIGTYPE (*old_int)(int);
    RETSIGTYPE (*old_quit)(int);
#endif /* ANSI_PROTOTYPES */

    old_int  = signal(SIGINT,  SIG_IGN);
    old_quit = signal(SIGQUIT, SIG_IGN);

    if (argc < 2)
	return 0;

    cpid = vfork();
    if (cpid < 0)
    {
	perror("shell command couldn't start");
	return 1;
    }
    else if (cpid == 0)
    {
	(void)signal(SIGINT,  SIG_DFL);
	(void)signal(SIGQUIT, SIG_DFL);
	if (execvp(argv[1], argv + 1) < 0)
	{
	    perror("shell command failed");
	    _exit(1);
	}
    }
    else
    {
	if (wait(&status) < 0)
	{
	    perror("waiting for shell command to finish");
	    return 1;
	}
	status >>= 8;
    }
    (void)signal(SIGINT,  old_int);
    (void)signal(SIGQUIT, old_quit);

    return status;
}
