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
COMMAND cat remote "cat the contents of remote files"
 *  ---INFOEND---  */

#include "client.h"
#include <setjmp.h>

#define FSPENVVAR "FSPFILE"

#ifndef ANSI_PROTOTYPES
extern char **glob();
#else /* ANSI_PROTOTYPES */
extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */

static int recursive;
static int use_pager;

static jmp_buf my_sigpipe_env;
static int longjmp_set, pipe_broken;

static RETSIGTYPE
#ifndef ANSI_PROTOTYPES
dont_die(sig)
    int sig;
#else /* ANSI_PROTOTYPES */
dont_die(int sig)
#endif /* ANSI_PROTOTYPES */
{
    pipe_broken = 1;
    client_intr_cnt++;

    if (longjmp_set)
	longjmp(my_sigpipe_env, 0);
}

static int
#ifndef ANSI_PROTOTYPES
cat_file(name, sbufp, depth)
    char *name;
    struct stat *sbufp;
    int depth;
#else /* ANSI_PROTOTYPES */
cat_file(char *name, struct stat *sbufp, int depth)
#endif /* ANSI_PROTOTYPES */
{
    int retval;
    FILE *catout;
#ifndef ANSI_PROTOTYPES
    RETSIGTYPE (*oldsig)();
#else /* ANSI_PROTOTYPES */
    RETSIGTYPE (*oldsig)(int);
#endif /* ANSI_PROTOTYPES */

    if (sbufp->st_size == 0)
    {
	ffprintf(STDOUT, "nothing to download for `%s'\n", name);
	return 0;
    }

    if (!validate_operation(name, UTIL_DOWNLOAD))
	return -1;

    longjmp_set = 0;
    pipe_broken = 0;
    oldsig = signal(SIGPIPE, dont_die);

    if (use_pager)
    {
#ifdef HAVE_PUTENV
	int l1, l2;
	static char *oldenv = 0;
	char *newenv;

	l1 = strlen(FSPENVVAR);
	l2 = strlen(name);

	newenv = (char *)malloc(l1 + l2 + 2);
	(void)strcpy(newenv, FSPENVVAR);
	newenv[l1] = '=';
	(void)strcpy(newenv + l1 + 1, name);
	newenv[l1 + l2 + 1] = '\0';

	(void)putenv(newenv);

	if (oldenv)
	    (void)free(oldenv);
	oldenv = newenv;
#endif

	catout = popen(pager_command, "w");
	if (catout == (FILE*)0)
	{
	    use_pager = 0;
	    ffprintf(STDERR, "?cat: cannot open pipe to command `%s'\n",
		     pager_command);
	    catout = STDOUT;
	}
    }
    else
	catout = STDOUT;

    retval = 0;

    if (pipe_broken)
	client_intr_cnt--;
    else
    {
	if (!setjmp(my_sigpipe_env))
	{
	    int bytes;
	    longjmp_set = 1;

	    bytes = util_download(name, fileno(catout), 0, sbufp->st_size);
	    if (bytes < 0)
		retval = 1;
	    else
		ffprintf(STDINFO, "downloaded %d bytes out of %d\n",
			 bytes, sbufp->st_size);
	}
	else
	    client_intr_cnt--;
    }

    if (use_pager)
	(void)pclose(catout);

    (void)signal(SIGPIPE, SIG_DFL);

    return retval;
}

static int
#ifndef ANSI_PROTOTYPES
do_cat_file(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_cat_file(char *name)
#endif /* ANSI_PROTOTYPES */
{
    if (!validate_operation(name, UTIL_PROCESS_FILE))
	return -1;

    return util_process_file(name, recursive, 0, cat_file, 0, 0);
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
    int retval, errcnt, ch;
    extern int optind, opterr;
    int client_trace_preserve = client_trace;

    recursive = 0;

    optind = 1;
    opterr = 1;
    errcnt = 0;

    while ((ch = getopt(argc, argv, "r")) != EOF)
        switch (ch)
        {
          case 'r':
            recursive = MAXRECURSION;
            break;
          default:
            errcnt++;
            break;
        }

    if (errcnt > 0)
        return 1;

    if (isatty(fileno(STDOUT)))
    {
	client_trace = 0;
	use_pager = (pager_command && *pager_command);
    }
    else
	use_pager = 0;

    retval = -util_process_arglist(argv + optind, do_cat_file);

    client_trace = client_trace_preserve;
    client_done();

    return retval;
}
