/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND pager none "set the pager for the `cat' command"
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
    if (pager_command)
	(void)free(pager_command);

    pager_command = (argc > 1)? strdup(argv[1]): 0;

    if (pager_command && *pager_command)
	ffprintf(STDINFO,"pager now set to `%s'\n", pager_command);
    else
	ffprintf(STDINFO,"pager turned off\n");

    return 0;
}
