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
COMMAND mkdir none "make a directory on the remote system"
 *  ---INFOEND---  */

#include "client.h"

static int dirty;

static int
#ifndef ANSI_PROTOTYPES
do_make_dir(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_make_dir(char *name)
#endif /* ANSI_PROTOTYPES */
{
    char *path;
    UBUF *ub;

    if (!validate_operation(name, DIR_MKDIR))
	return -1;

    path = util_abs_path(name);
    ub = client_interact(CC_MAKE_DIR, 0L, strlen(path), path + 1, 0, NULLP);
    (void)free(path);

    if (client_intr_state > 1 || !ub)
	return -1;

    if (ub->cmd == CC_ERR)
    {
	ffprintf(STDERR, "mkdir: cannot create `%s'\n", name);
	return(-1);
    }

    dirty = 1;
    ffprintf(STDINFO, "%s\t: %s\n", name, ub->buf);

    return 0;
}

/* ARGSUSED */
int
#ifndef ANSI_PROTOTYPES
main(argc, argv,envp)
    int argc;
    char *argv[];
    char *envp[];
#else /* ANSI_PROTOTYPES */
main(int argc, char **argv, char **envp)
#endif /* ANSI_PROTOTYPES */
{
    int retval = 0;

    dirty = 0;

    for (argv++; *argv; argv++)
	retval |= (do_make_dir(*argv) < 0);

    if (dirty)
	util_dirtydir(".");

    client_done();

    return retval;
}
