/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND datestamp none "set whether date stamps are set on downloaded files"
 *  ---INFOEND---  */

#include "client.h"

int datestamp = 0;

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
	datestamp = !datestamp;
    else
    {
	if (strcmp(argv[1], "false") == 0)
	    datestamp = 0;
	else if (strcmp(argv[1], "true") == 0)
	    datestamp = 1;
	else if (strcmp(argv[1], "?") != 0)
	    datestamp = (atoi(argv[1]) != 0);
    }

    ffprintf(STDINFO,"file date stamps will%s be preserved on downloads\n",
	     datestamp? "": " not");

    return 0;
}
