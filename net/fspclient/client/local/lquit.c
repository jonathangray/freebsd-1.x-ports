/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND quit none "quit (what did you expect?)"
COMMAND exit none "exit with return code of last command, or code given"
 *  ---INFOEND---  */

#include "client.h"
#include "main.h"

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
    int exitcode;

    notquit = 0;
    if (strcmp(argv[0], "quit") == 0)
	exitcode = 0;
    else
	exitcode = (argc > 1)? atoi(argv[1]): last_retcode;

    return exitcode;
}
