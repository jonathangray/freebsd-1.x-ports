/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND lcd local "local cd"
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
    char *newldir, **files;

    if (argc < 2)
    {
	if (STDPROMPT)
	{
	    ffprintf(STDPROMPT,"(dir) ");
	    newldir = readword();
	}
	else
	    newldir = 0;
    }
    else
	newldir = argv[1];

    if (newldir == 0)
	return 1;

    if ((files = glob(newldir)) != 0)
	newldir = files[0];

    if (chdir(newldir) < 0)
    {
	if (files)
	    free_glob(files);
        perror("lcd");
	return 1;
    }

    if (files)
	free_glob(files);

    return 0;
}
