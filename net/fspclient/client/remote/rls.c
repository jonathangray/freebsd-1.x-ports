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
COMMAND ls remote "list the contents of the remote directory"
 *  ---INFOEND---  */

#include "client.h"
#include <setjmp.h>

extern int fls_main();
/* extern int fls_main(int argc, char **argv, char **envp); */

static jmp_buf main_env;

void
#ifndef ANSI_PROTOTYPES
ls_bad(n)
    int n;
#else /* ANSI_PROTOTYPES */
ls_bad(int n)
#endif /* ANSI_PROTOTYPES */
{
    longjmp(main_env, n);
}

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
    int retval;
    char *old_env_dir;

    old_env_dir = strdup(env_dir);

    /* assume fls_main() does the appropriate validate_operation() */
    if (!(retval = setjmp(main_env)))
	retval = fls_main(argc, argv, envp);

    (void)free(env_dir);
    env_dir = old_env_dir;

    client_done();

    return retval;
}
