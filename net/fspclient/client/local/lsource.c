/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND source local "source the contents of a file"
 *  ---INFOEND---  */

#include "client.h"
#include "main.h"

static int
#ifndef ANSI_PROTOTYPES
do_source(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_source(char *name)
#endif /* ANSI_PROTOTYPES */
{
    int retval;
    iobuffers storedfs;
    FILE *myin, *myout;
    char *myargv[3];

    if ((myin = fopen(name, "r")) == 0)
    {
	ffprintf(STDERR, "source: can't open `%s'\n", name);
	return -1;
    }

    if ((myout = fopen("/dev/null", "w")) == 0)
    {
	(void)fclose(myin);
	ffprintf(STDERR, "source: can't open `/dev/null'!!\n");
	return -1;
    }

    (void)fflush(STDOUT);
    (void)fflush(STDERR);

    storedfs = global_iobuffers;
    STDIN  = myin;
    STDOUT = STDINFO = STDWARN = STDPROMPT = myout;

    myargv[0] = "source";
    myargv[1] = name;
    myargv[2] = 0;

    retval = execute_stdin(2, myargv);

    global_iobuffers = storedfs;

    (void)fclose(myout);
    (void)fclose(myin);

    return -retval;
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
    if (argc < 2)
    {
	ffprintf(STDERR, "source: need filename\n");
	return 1;
    }

    return util_process_arglist(argv + 1, do_source);
}
