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
COMMAND grab remote "download and delete files from the remote system"
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
grab_file(name, sbufp, depth)
    char *name;
    struct stat *sbufp;
    int depth;
#else /* ANSI_PROTOTYPES */
grab_file(char *name, struct stat *sbufp, int depth)
#endif /* ANSI_PROTOTYPES */
{
    char *path;
    int fd, retval = 0;

    path = strrchr(name, '/');
    if (path)
	path++;
    else
	path = name;

    if ((fd = open(path, O_WRONLY | O_TRUNC | O_CREAT, 0644)) < 0)
    {
	retval = 1;
	ffprintf(STDERR,"grab: cannot write local file `%s': %s\n",
	         path, sys_errlist[errno]);
    }
    else
    {
	int bytes;

	bytes = util_grab_file(name, fd, 0, sbufp->st_size);
	(void)close(fd);

	if (bytes < sbufp->st_size)
	{
	    retval = 1;
	    (void)unlink(path);
	}
	else
	    dirty = 1;
    }

    return -retval;
}

static int
#ifndef ANSI_PROTOTYPES
do_grab_file(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_grab_file(char *name)
#endif /* ANSI_PROTOTYPES */
{
    if (!validate_operation(name, UTIL_GRAB_FILE | UTIL_PROCESS_FILE))
	return -1;

    return util_process_file(name, 0, 0, grab_file, 0, 0);
}

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

    if (argc > 1)
	retval = -util_process_arglist(argv + 1, do_grab_file);
    else
    {
#define INBUF_SIZE 1024
	char *argbuf[2], buf[INBUF_SIZE];

	argbuf[0] = buf;
	argbuf[1] = 0;

	retval = 0;

	while (client_intr_state < 2)
	{
	    char *eofn;

	    ffprintf(STDPROMPT, "(grab) ");
	    if (!my_fgets(buf, INBUF_SIZE, STDIN) || !buf[0])
		break;

	    if (buf[0] == '\n')
		continue;

	    eofn = strrchr(buf, '\n');
	    if (eofn)
		*eofn = 0;

	    retval |= (util_process_arglist(argbuf, do_grab_file) < 0);
	}
    }

    if (dirty)
	util_dirtydir(".");

    client_done();

    return retval;
}
