/******************************************************************************
* This file is Copyright 1993 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND du remote "calculate the remote disk usage"
 *  ---INFOEND---  */

#include "client.h"

#ifndef ANSI_PROTOTYPES
extern char **glob();
#else /* ANSI_PROTOTYPES */
extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */

static int recursive;
static int want_all, want_summary;
static int cwdlen;
static char *abscwd;
static u_long grand_total, filcnt;

#define DUBLOCKSIZE 1024

static u_int
#ifndef ANSI_PROTOTYPES
block_convert(size)
    u_int size;
#else /* ANSI_PROTOTYPES */
block_convert(u_int size)
#endif /* ANSI_PROTOTYPES */
{
    return (size + DUBLOCKSIZE - 1) / DUBLOCKSIZE;
}

static char *
#ifndef ANSI_PROTOTYPES
strip_cwd(name, type, rel)
    char *name;
    int type;
    int *rel;
#else /* ANSI_PROTOTYPES */
strip_cwd(char *name, int type, int *rel)
#endif /* ANSI_PROTOTYPES */
{
    if (strncmp(abscwd, name, cwdlen) == 0)
    {
	/* it's probably a relative pathname... */
	*rel = 1;

	if (type == 1 && name[cwdlen] == 0)
	    return "";

	if (name[cwdlen] == '/')
	    return name + cwdlen;

	/* oh well, maybe not... */
    }

    *rel = 0;

    if (name[0] == '/' && name[1] == 0)
	return "";

    return name;
}

static int
#ifndef ANSI_PROTOTYPES
add_file_size(name, sbufp, depth)
    char *name;
    struct stat *sbufp;
    int depth;
#else /* ANSI_PROTOTYPES */
add_file_size(char *name, struct stat *sbufp, int depth)
#endif /* ANSI_PROTOTYPES */
{
    if (want_all || depth == 0)
    {
	char *sname;
	int rel;

        sname = strip_cwd(name, 0, &rel);

        ffprintf(STDOUT, "%-7d %s%s\n", block_convert(sbufp->st_size),
		 rel? ".": "", sname);
    }

    grand_total += sbufp->st_size;

    return 0;
}

static int
#ifndef ANSI_PROTOTYPES
enter_dir(name, depth, pdata)
    char *name;
    int depth;
    char **pdata;
#else /* ANSI_PROTOTYPES */
enter_dir(char *name, int depth, char **pdata)
#endif /* ANSI_PROTOTYPES */
{
    int *curtotal;

    /*
    ** there is no point trying to process this directory if it can't be
    ** read -- check the directory for readability
    */
    if (!validate_operation(name, LITERAL_DIR | UTIL_DIR))
	return -1;

    /*
    ** set up some private data space to hold the current file total on
    ** entry to a directory; this is needed so we can print the total when
    ** the directory is left
    */
    curtotal = (int *)malloc(sizeof(int));
    *curtotal = grand_total;
    *pdata = (char *)curtotal;

    return 0;
}

static void
#ifndef ANSI_PROTOTYPES
leave_dir(name, depth, data)
    char *name;
    int depth;
    char *data;
#else /* ANSI_PROTOTYPES */
leave_dir(char *name, int depth, char *data)
#endif /* ANSI_PROTOTYPES */
{
    if (!want_summary || depth == 0)
    {
	char *sname;
	int rel;

	sname = strip_cwd(name, 1, &rel);
        ffprintf(STDOUT, "%-7d %s%s/\n",
	         block_convert(grand_total - *((int *)data)),
		 rel? ".": "", sname);
    }

    /* data will be free()'d by util_process_file() */
}

static int
#ifndef ANSI_PROTOTYPES
do_du(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_du(char *name)
#endif /* ANSI_PROTOTYPES */
{
    if (!validate_operation(name, UTIL_PROCESS_FILE))
	return -1;

    filcnt++;
    return util_process_file(name, recursive, 0, add_file_size,
			     enter_dir, leave_dir);
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
    static char *here[2] = { ".", 0 };
    int want_total;
    char **argvec;

    recursive = 0;
    want_all = 0;
    want_total = 0;
    want_summary = 0;

    optind = 1;
    opterr = 1;
    errcnt = 0;

    /* options in the style of GNU du ... well, sort of */
    while ((ch = getopt(argc, argv, "acrs")) != EOF)
	switch (ch)
	{
	  case 'a':
	    want_all = 1;
	    break;
	  case 'c':
	    want_total = 1;
	    break;
	  case 'r':
	    recursive = MAXRECURSION;
	    break;
	  case 's':
	    want_summary = 1;
	    break;
	  default:
	    errcnt++;
	    break;
	}

    if (errcnt > 0)
	ffprintf(STDERR, "?du usage: du [-[acrs]] [files...]\n");

    /* cross validate options */
    if (want_summary && want_all)
    {
	ffprintf(STDERR, "?du: cannot both summarize and show all entries\n");
	errcnt = 1;
    }

    /* if any errors in the options, return an error */
    if (errcnt > 0)
    	return 1;

    filcnt = 0;
    grand_total = 0;

    /* special case `du' without arguments -- becomes `du .' */
    argvec = (optind == argc)? here: argv + optind;

    /*
    ** if there is only one argument and no recursion has been requested,
    ** then allow it to be a directory and we will set recursive to be 1
    */
    if (recursive == 0 && optind >= argc - 1)
	recursive = 1;

    abscwd = util_abs_path(env_dir);
    cwdlen = strlen(abscwd);
    /* special case "/" */
    if (cwdlen == 1)
        cwdlen = 0;
    retval = -util_process_arglist(argvec, do_du);
    (void)free(abscwd);

    if (want_total && filcnt > 1)
    {
	ffprintf(STDOUT, "--------:------\n");
	ffprintf(STDOUT, "%-7d   TOTAL\n", block_convert(grand_total));
    }

    client_done();

    return retval;
}
