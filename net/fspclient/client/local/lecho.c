/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND echo none "echo a string with newline"
COMMAND echon none "echo a string without newline"
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
    int i;

    for (i = 1; i < argc; i++)
	ffprintf(STDOUT, "%s%s", (i > 1)?" ":"", argv[i]);

    if (strcmp(argv[0], "echon") != 0)
	ffprintf(STDOUT, "\n");

    return 0;
}
