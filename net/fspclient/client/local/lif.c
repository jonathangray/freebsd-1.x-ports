/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND iferror none "perform an operation if the last command had an error"
COMMAND ifok none "perform an operation if the last command had no error"
COMMAND onerror none "the operation to perform after any command causes an error"
 *  ---INFOEND---  */

#include "client.h"
#include "main.h"

char **onerrorargv = 0;
int onerrorargc = 0;

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
    if ((strcmp(argv[0], "iferror") == 0 && last_retcode)
       || (strcmp(argv[0], "ifok") == 0 && last_retcode == 0))
    {
	execute_command(argc - 1, argv + 1);
	return last_retcode;
    }
    else if (strcmp(argv[0], "onerror") == 0)
    {
	if (onerrorargv)
	{
	    char **args;

	    for (args = onerrorargv; *args; args++)
		(void)free(*args);

	    (void)free((char *)onerrorargv);
	}

	if (argc < 2)
	{
	    onerrorargc = 0;
	    onerrorargv = 0;
	}
	else
	{
	    int i;

	    onerrorargv = (char **)malloc(sizeof(char *) * argc);
	    for (i = 1; argv[i] && i < argc; i++)
		onerrorargv[i - 1] = strdup(argv[i]);

	    onerrorargc = i - 1;
	    onerrorargv[onerrorargc] = 0;
	}
    }

    return 0;
}
