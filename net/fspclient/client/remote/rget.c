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
COMMAND get remote "download files from the remote system"
 *  ---INFOEND---  */

#include "client.h"

#ifdef HAVE_UTIME_H
# include <utime.h>
#else
# ifdef HAVE_UTIMES_H
#  include <utimes.h>
#  define utime utimes
# else
#  define NO_UTIME
# endif
#endif

#ifndef ANSI_PROTOTYPES
extern char **glob();
#else /* ANSI_PROTOTYPES */
extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */

static int baselen, cwdlen;
static char *abscwd;
static int recursive;

typedef enum { GET_WHOLE, GET_HEAD, GET_MIDDLE, GET_TAIL } GetOp;
static GetOp fspgetmode;

static int
#ifndef ANSI_PROTOTYPES
get_file(name, sbufp, depth)
    char *name;
    struct stat *sbufp;
    int depth;
#else /* ANSI_PROTOTYPES */
get_file(char *name, struct stat *sbufp, int depth)
#endif /* ANSI_PROTOTYPES */
{
    char *basename, *path, *remname;
    char *tmpname, *openname, *modedesc;
    int fd, pathlen, mode, retval = 0;

    if (!validate_operation(name, UTIL_DOWNLOAD))
	return -1;

    basename = strrchr(name, '/');
    if (basename)
	basename++;
    else
	basename = name;

    if (recursive)
	path = name + baselen;
    else
	path = basename;

    pathlen = strlen(path);
    tmpname = (char *)malloc(5 + pathlen);

    if (recursive)
    {
	int dirlen = (int)(basename - path);
	(void)strncpy(tmpname, path, dirlen);
	(void)strcpy(tmpname + dirlen, ".in.");
	(void)strcpy(tmpname + dirlen + 4, basename);
    }
    else
    {
	(void)strcpy(tmpname, ".in.");
	(void)strcpy(tmpname + 4, path);
    }

    if (baselen > cwdlen && strncmp(abscwd, name, cwdlen) == 0)
	remname = name + cwdlen + 1;
    else
	remname = name;

    switch (fspgetmode)
    {
      case GET_WHOLE:
	if (access(path, W_OK) == 0)
	{
	    openname = path;
	    mode     = O_WRONLY | O_CREAT | O_APPEND;
	    modedesc = "update";
	}
	else if (access(tmpname, W_OK) == 0)
	{
	    openname = tmpname;
	    mode     = O_WRONLY | O_CREAT | O_APPEND;
	    modedesc = "update";
	}
	else
	{
	    openname = tmpname;
	    mode     = O_WRONLY | O_CREAT | O_TRUNC;
	    modedesc = "writing";
	}
	break;

      case GET_HEAD:
      case GET_MIDDLE:
      case GET_TAIL:
	openname = tmpname;
	mode     = O_WRONLY | O_CREAT | O_TRUNC;
	modedesc = "writing";
	break;
    }

    if ((fd = open(openname, mode, 0644)) < 0)
    {
	retval = 1;
	ffprintf(STDERR, "?get: cannot open local `%s' for %s: %s\n",
		 path, modedesc, sys_errlist[errno]);
    }
    else
    {
	int can_rename = 1, pos = 0, bytes;
	struct stat lbuf;

	if (openname != tmpname && rename(path, tmpname) < 0)
	{
	    ffprintf(STDERR, "?get: failed to rename `%s' to `%s': %s\n",
		     path, tmpname, sys_errlist[errno]);
	    can_rename = 0;
	}

	(void)fstat(fd, &lbuf);

	if (S_ISREG(lbuf.st_mode))
	{
	    if ((pos = lseek(fd, 0L, SEEK_END)) > 0 && askprompt && STDPROMPT)
	    {
		char result, buf[256];

		ffprintf(STDPROMPT,
		"`%s' already contains %d bytes; continue or restart [cr]? ",
		openname, pos);

		(void)my_fgets(buf, 256, STDIN);

		/* if ^C was hit, then return immediately */
		if (buf[0] == '\0')
		{
		    (void)free(tmpname);
		    return 0;
		}

		if (sscanf(buf, " %c", &result) != 1 || strchr("rRnN", result))
		{
		    pos = 0;
		    (void)ftruncate(fd, 0);
		    (void)lseek(fd, 0L, SEEK_SET);
		}
	    }
	}

	if (client_trace >= 2)
	{
	    if (pos >= sbufp->st_size)
		ffprintf(STDINFO, "nothing to download for `%s' -- skipping\n",
			 remname);
	    else if (pos > 0)
		ffprintf(STDINFO, "resuming `%s' (%d bytes remaining)\n",
			 remname, sbufp->st_size - pos);
	    else
		ffprintf(STDINFO, "downloading `%s' (%d bytes)\n",
			 remname, sbufp->st_size);
	}

	bytes = util_download(remname, fd, pos, sbufp->st_size - pos);
	if (bytes < 0)
	{
	    retval = 1;
	    (void)close(fd);
	    (void)unlink(can_rename? tmpname: path);
	}
	else
	{
	    (void)close(fd);

	    if (bytes < sbufp->st_size - pos)
		ffprintf(STDERR, "?failed to get %d bytes -- file truncated\n",
			 sbufp->st_size - pos - bytes);

	    if (can_rename && rename(tmpname, path) < 0)
		ffprintf(STDERR, "?get: failed to rename `%s' to `%s': %s\n",
				tmpname, path, sys_errlist[errno]);
#ifndef NO_UTIME
	    else if (datestamp)
	    {
		struct stat rfstat;

		if (util_stat(name, &rfstat) < 0)
		    ffprintf(STDERR, "?can't get times for file `%s': %s\n",
			     name, sys_errlist[errno]);
		else
		{
		    struct utimbuf lftimes;

		    lftimes.actime  = rfstat.st_atime;
		    lftimes.modtime = rfstat.st_mtime;

		    if (utime(path, &lftimes) < 0)
			ffprintf(STDERR, "?can't date stamp file `%s': %s\n",
				 path, sys_errlist[errno]);
		}
	    }
#endif
	}

	(void)free(tmpname);
    }

    return -retval;
}

static int
#ifndef ANSI_PROTOTYPES
make_dir(name, depth, pdata)
    char *name;
    int depth;
    char **pdata;
#else /* ANSI_PROTOTYPES */
make_dir(char *name, int depth, char **pdata)
#endif /* ANSI_PROTOTYPES */
{
    struct stat sbuf;

    if (stat(name + baselen, &sbuf) == 0)
    {
	/* check if the directory already exists... */
	if (S_ISDIR(sbuf.st_mode))
	    return 0;

	/* the directory doesn't exist, but *something* does... urgh! */
	ffprintf(STDERR, "?local file `%s' is not a directory\n",
		 name + baselen);
	return -1;
    }

    /* nothing exists by this name -- try to create it */
    if (mkdir(name + baselen, 0755) < 0)
    {
	ffprintf(STDERR, "?can't make directory `%s': %s\n",
	         name + baselen, sys_errlist[errno]);
	return -1;
    }

    return 0;
}

static int
#ifndef ANSI_PROTOTYPES
do_get(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_get(char *name)
#endif /* ANSI_PROTOTYPES */
{
    char *dirname;

    if (!validate_operation(name, UTIL_PROCESS_FILE | UTIL_STAT))
	return -1;

    dirname = util_abs_path(name);
    /* util_abs_path() is guaranteed to return path with a '/' */
    baselen = strrchr(dirname, '/') - dirname + 1;
    (void)free(dirname);

    return util_process_file(name, recursive, 0, get_file, make_dir, 0);
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

    recursive = 0;

    optind = 1;
    opterr = 1;
    errcnt = 0;

    if (strcmp(argv[0], "head") == 0)
	fspgetmode = GET_HEAD;
    else if (strcmp(argv[0], "tail") == 0)
	fspgetmode = GET_TAIL;
    else
	fspgetmode = GET_WHOLE;

    while ((ch = getopt(argc, argv, "ftnr")) != EOF)
        switch (ch)
        {
	  case 'f':
          case 'r':
            recursive = MAXRECURSION;
            break;
          default:
            errcnt++;
            break;
        }

    if (errcnt > 0)
        return 1;

#if 0
    if (fspgetmode == GET_WHOLE && (frompos != 0 || getlen XXX))
	;
#endif

    abscwd = util_abs_path(env_dir);
    cwdlen = strlen(abscwd);
    /* special case "/" */
    if (cwdlen == 1)
	cwdlen = 0;

    if (argc > optind)
	retval = -util_process_arglist(argv + optind, do_get);
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

	    ffprintf(STDPROMPT, "(get) ");
	    if (!my_fgets(buf, INBUF_SIZE, STDIN) || !buf[0] || buf[0] == '\n')
		break;

	    eofn = strrchr(buf, '\n');
	    if (eofn)
		*eofn = 0;

	    retval |= (util_process_arglist(argbuf, do_get) < 0);
	}
    }

    (void)free(abscwd);

    client_done();

    return retval;
}
