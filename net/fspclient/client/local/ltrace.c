/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND trace none "set trace mode to on, off, or hash"
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
    if (argc < 2)
	client_trace = (client_trace + 1) % 4;
    else
    {
	if (strcmp(argv[1], "off") == 0)
	    client_trace = 0;
	else if (strcmp(argv[1], "on") == 0)
	    client_trace = 1;
	else if (strcmp(argv[1], "hash") == 0)
	    client_trace = 2;
	else if (strcmp(argv[1], "all") == 0)
	    client_trace = 3;
	else if (strcmp(argv[1], "?") != 0)
	    client_trace = atoi(argv[1]);
    }

    if (client_trace < 0)
	client_trace = 0;
    else if (client_trace > 3)
	client_trace = 3;

    ffprintf(STDINFO,
	     "client tracing is %s\n",
	     (client_trace == 0)? "off":
	     (client_trace == 1)? "on":
	     (client_trace == 2)? "hashing":
	     (client_trace == 3)? "all":
	     "weird");

    return 0;
}
