    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *  Copyright (c) 1993 by Phil Richards (pgr@prg.ox.ac.uk)             *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND cd remote "change the remote directory"
 *  ---INFOEND---  */

#include "client.h"

#ifndef ANSI_PROTOTYPES
extern char **glob();
#else /* ANSI_PROTOTYPES */
extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */

/* ARGSUSED */
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
    int retval = 0;

    if (argc == 1)
    {
	/* for `cd' do `cd /' -- assume this _must_ work */
	if (env_dir)
	    (void)free(env_dir);
	env_dir = strdup("/");
    }
    else
    {
	char **globdata, **files, *singlefile[2], *newcwd;

        if (!(globdata = files = glob(argv[1])))
	{
	    files         = singlefile;
	    singlefile[0] = argv[1];
	    singlefile[1] = 0;
	}

	newcwd = util_abs_path(*files);

	if (globdata)
	    free_glob(globdata);

	/*
	** no need to protect this with a validate_operation() since it
	** does no more than validate_operation() does
	*/
	retval = util_print_protection(newcwd);

	if (retval < 0)
	    (void)free(newcwd);
	else
	{
	    if (env_dir)
		(void)free(env_dir);
	    env_dir = newcwd;
	}
    }

    if (standalone)
	(void)puts(env_dir);

    client_done();

    return -retval;
}
