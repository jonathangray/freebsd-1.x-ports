/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND skipto none "skip all input until the supplied label is reached"
 *  ---INFOEND---  */

#include "client.h"
#include "main.h"

char *skiptolabel = 0;

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
    if (argc != 2)
    {
	ffprintf(STDERR, "?skipto: a single label must be specified\n");
	return 1;
    }

    skiptolabel = strdup(argv[1]);

    return 0;
}
