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
COMMAND put local "upload files to the remote system"
 *  ---INFOEND---  */

#include "client.h"

static int dirty;

static int
#ifndef ANSI_PROTOTYPES
put_file(name)
    char *name;
#else /* ANSI_PROTOTYPES */
put_file(char *name)
#endif /* ANSI_PROTOTYPES */
{
    struct stat sbuf;
    char *path;
    int retval = 0;
    FILE *fp;

    if (stat(name, &sbuf) != 0)
    {
	perror(name);
	return -1;
    }

    if (!S_ISREG(sbuf.st_mode))
    {
	ffprintf(STDERR,"put: `%s' is not a file\n", name);
	return -1;
    }

    if (!validate_operation(name, UTIL_UPLOAD))
	return -1;

    path =  strrchr(name, '/');
    if (path)
	path++;
    else
	path = name;

    if ((fp = fopen(name, "r")))
    {
	if (util_upload(path, fp) < 0)
	{
	    retval = 1;
	    dirty  = 1;
	}

	(void)fclose(fp);
    }
    else
    {
	retval = 1;
	ffprintf(STDERR,"put: cannot read `%s'\n", name);
    }

    return -retval;
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

    if(argc > 1)
	retval = -util_process_arglist(argv + 1, put_file);
    else
    {
	retval = 0;

	while (client_intr_state < 2)
	{
#define INBUF_SIZE 1024
	    char buf[INBUF_SIZE], *eofn;

	    ffprintf(STDPROMPT, "(put) ");
	    if (!my_fgets(buf, INBUF_SIZE, STDIN) || !buf[0])
		break;

	    if (buf[0] == '\n')
		continue;

	    eofn = strrchr(buf, '\n');
	    if (eofn)
		*eofn = 0;

	    retval |= (put_file(buf) < 0);
	}
    }

    if (dirty)
	util_dirtydir(".");

    client_done();

    return retval;
}
