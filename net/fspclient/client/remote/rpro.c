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
COMMAND pro remote "change the permissions on a remote directory"
 *  ---INFOEND---  */

#include "client.h"

#ifndef ANSI_PROTOTYPES
extern char **glob();
#else /* ANSI_PROTOTYPES */
extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */
static char *key;

static int
#ifndef ANSI_PROTOTYPES
set_pro(name, key)
    char *name, *key;
#else /* ANSI_PROTOTYPES */
set_pro(char *name, char *key)
#endif /* ANSI_PROTOTYPES */
{
    char *path;
    UBUF *ub;

    path = util_abs_path(name);
    ub = client_interact(CC_SET_PRO, 0L, strlen(path), path + 1,
			 strlen(key) + 1, key);
    (void)free(path);

    if (client_intr_state > 1 || !ub || ub->cmd == CC_ERR)
	return -1;

    util_dirtypro(name);

    return util_print_protection(name);
}

static int
#ifndef ANSI_PROTOTYPES
do_pro(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_pro(char *name)
#endif /* ANSI_PROTOTYPES */
{
    if (key)
    {
	if (!validate_operation(name, DIR_OWNER | LITERAL_DIR))
	    return -1;
	return set_pro(name, key);
    }
    else
    {
	/* nothing that can be validated */
	return util_print_protection(name);
    }
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
    static char *here[2] = { ".", 0 };

    /* if the option is "-" then take the rest as a list of files */
    /* pass *any* option onto the server */
    if (argv[1]
       && (argv[1][0] == '+' || argv[1][0] == '-')
       && argv[1][1] != '\0')
    {
	argv++;
        key = argv[0];
    }
    else
	key = 0;

    if (!argv[1])
	argv = here;
    else
	argv++;

    retval = -util_process_arglist(argv, do_pro);

    client_done();

    return retval;
}
