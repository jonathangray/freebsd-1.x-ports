/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND readme none "read the README; set readme mode to never, once, or always"
 *  ---INFOEND---  */

extern int readme_max;

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
	util_print_readme();
    else
    {
	if (strcmp(argv[1], "never") == 0 || strcmp(argv[1], "0") == 0)
	    readme_max = 0;
	else if (strcmp(argv[1], "once") == 0 || strcmp(argv[1], "1") == 0)
	    readme_max = 1;
	else if (strcmp(argv[1], "always") == 0 || strcmp(argv[1], "2") == 0)
	    readme_max = -1;
	else
	{
	    ffprintf(STDERR, "?usage: readme [never|once|always]\n");
	    return 1;
	}
    }

    return 0;
}
