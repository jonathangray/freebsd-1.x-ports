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
COMMAND rm remote "remove files from the remote system"
 *  ---INFOEND---  */

#include "client.h"

#ifndef ANSI_PROTOTYPES
extern char **glob();
#else /* ANSI_PROTOTYPES */
extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */

static int dirty;

static int
#ifndef ANSI_PROTOTYPES
do_rm(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_rm(char *name)
#endif /* ANSI_PROTOTYPES */
{
    char *op;
    UBUF *ub;
    struct stat sbuf;

    if (!validate_operation(name, DIR_DEL))
	return -1;

    if (util_stat(name, &sbuf) < 0)
    {
	ffprintf(STDERR,"rm: cannot remove `%s': no such file\n", name);
	return -1;
    }

    if (!S_ISREG(sbuf.st_mode))
    {
	ffprintf(STDERR,"rm: cannot remove `%s': not a file\n", name);
	return -1;
    }

    op = util_abs_path(name);
    ub = client_interact(CC_DEL_FILE, 0L, strlen(op), op+1, 0, NULLP);
    (void)free(op);

    if (client_intr_state > 1 || !ub)
	return -1;

    if (ub->cmd == CC_ERR)
    {
	ffprintf(STDERR,"rm: cannot remove `%s'\n", name);
	return -1;
    }

    dirty = 1;

    return 0;
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

    dirty = 0;

    retval = -util_process_arglist(argv + 1, do_rm);

    if (dirty)
	util_dirtydir(".");

    client_done();

    return retval;
}
